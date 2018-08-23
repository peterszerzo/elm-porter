module Porter exposing
    ( Config
    , Model, Msg, init, update, subscriptions
    , Request
    , request, andThen, send
    )

{-| Port message manager to emulate a request-response style communication through ports, a'la `Http.send ResponseHandler request`.


# Configuration

@docs Config


# The setup

@docs Model, Msg, init, update, subscriptions


# Send messages

@docs Request
@docs request, andThen, send

-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Porter.Internals exposing (Msg(..), MultiRequest(..), Request(..), RequestWithHandler(..))
import Task


type alias MsgId =
    Int


{-| Porter model, used to keep track of response handlers.
-}
type Model req res msg
    = Model
        { handlers : ResponseHandlerMap req res msg
        , nextId : Int
        }


{-| Porter configuration, containing:

  - ports
  - message encoders/decoders.
  - the message that porter will use for its internal communications

-}
type alias Config req res msg =
    Porter.Internals.Config req res msg


{-| Initialize model.
-}
init : Model req res msg
init =
    Model
        { handlers = Dict.empty
        , nextId = 0
        }


type alias ResponseHandlerMap req res msg =
    Dict.Dict MsgId (RequestWithHandler req res msg)


encode : (req -> Encode.Value) -> MsgId -> req -> Encode.Value
encode encodeReq id msg =
    Encode.object
        [ ( "id", Encode.int id )
        , ( "msg", encodeReq msg )
        ]


{-| Module messages.
-}
type alias Msg req res msg =
    Porter.Internals.Msg req res msg


{-| Internal type used by requests that have a response handler.
-}
type alias RequestWithHandler req res msg =
    Porter.Internals.RequestWithHandler req res msg


{-| Opaque type of a 'request'. Use the `request` function to create one,
chain them using `andThen` and finally send it using `send`.
-}
type alias Request req res =
    Porter.Internals.Request req res


{-| Subscribe to messages from ports.
-}
subscriptions : Config req res msg -> Sub msg
subscriptions config =
    config.incomingPort Receive
        |> Sub.map config.porterMsg


{-| Starts a request that can be sent at a later time using `send`,
and that can be combined using `andThen`.
-}
request : req -> Request req res
request req =
    Request req []


{-| Chains two Porter requests together:
Run a second one right away when the first returns using its result in the request.
-}
andThen : (res -> Request req res) -> Request req res -> Request req res
andThen reqfun (Request initialReq reqfuns) =
    Request initialReq (reqfun :: reqfuns)


{-| Sends a request earlier started using `request`.
-}
send : Config req res msg -> (res -> msg) -> Request req res -> Cmd msg
send config responseHandler req =
    Porter.Internals.send config responseHandler req


{-| In theory, Elm Ints can go as high as 2^53, but it's safer in the long
run to limit them to 2^32.
-}
maxSignedInt : Int
maxSignedInt =
    2147483647


{-| Given what we think the next ID shoud be, return a safe ID, where:

  - the ID wraps back to 0 after the safe max Int
  - we check that the proposed ID is unused.

We would only absolutely need to check if the ID had been used if we've wrapped
around, so we could keep track of that if it made any real performance
difference.

-}
safeId : Int -> Dict Int a -> Int
safeId proposed used =
    if proposed >= maxSignedInt then
        safeId 0 used

    else if Dict.member proposed used then
        safeId (proposed + 1) used

    else
        proposed


{-| Update port model based on local messages. Returns a new port model and a local command that must be mapped to the parent message.
-}
update : Config req res msg -> Msg req res msg -> Model req res msg -> ( Model req res msg, Cmd msg )
update config msg (Model model) =
    case msg of
        SendWithNextId req ->
            let
                id =
                    safeId model.nextId model.handlers

                extractMsg (RequestWithHandler message _ _) =
                    message
            in
            ( Model
                -- We remember the next ID we ought to propose, though we
                -- will check it then before using it.
                { handlers = Dict.insert id req model.handlers
                , nextId = id + 1
                }
            , config.outgoingPort (encode config.encodeRequest id (extractMsg req))
            )

        Receive val ->
            val
                |> Decode.decodeValue
                    (Decode.map2 (\id res -> ( id, res ))
                        (Decode.field "id" Decode.int)
                        (Decode.field "msg" config.decodeResponse)
                    )
                |> Result.map
                    (\( id, res ) ->
                        Dict.get id model.handlers
                            |> Maybe.map (handleResponse config (Model model) id res)
                            |> Maybe.withDefault ( Model model, Cmd.none )
                    )
                |> Result.withDefault ( Model model, Cmd.none )

        ResolveChain multiRequest ->
            ( Model model, Porter.Internals.multiSend config identity multiRequest )


{-| Internal function that chains the steps of a RequestWithHandler after one another.
-}
handleResponse : Config req res msg -> Model req res msg -> MsgId -> res -> RequestWithHandler req res msg -> ( Model req res msg, Cmd msg )
handleResponse config (Model model) id res (RequestWithHandler msg mappers finalResponseHandler) =
    case mappers of
        [] ->
            ( Model { model | handlers = Dict.remove id model.handlers }
            , Task.perform finalResponseHandler (Task.succeed res)
            )

        mapper :: mappersTail ->
            let
                mappedResponse =
                    mapper res

                extractMsg (Request unwrappedMsg _) =
                    msg

                extractMappers (Request _ reqMappers) =
                    reqMappers
            in
            ( Model { model | handlers = Dict.remove id model.handlers }
            , Porter.Internals.runSendRequest config (RequestWithHandler (extractMsg mappedResponse) (extractMappers mappedResponse ++ mappersTail) finalResponseHandler)
            )