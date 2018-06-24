module Porter exposing
    (Model
    , Config
    , Msg
    , subscriptions
    , init
    , update
    , send
    , request
    , andThen
    , sendRequest
    , map
    , map2
    , map3
    )

{-| Port message manager to emulate a request-response style communication through ports, a'la `Http.send ResponseHandler request`.


# Configuration

@docs Config


# The setup

@docs Model, Msg, init, update, subscriptions


# Send messages

@docs send

# Build a complex chain of requests and finally send it

@docs request, andThen, sendRequest, map, map2, map3

-}

import Dict exposing (Dict)
import Task
import Json.Encode as Encode
import Json.Decode as Decode


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
    { outgoingPort : Encode.Value -> Cmd msg
    , incomingPort : (Encode.Value -> Msg req res msg) -> Sub (Msg req res msg)
    , encodeRequest : req -> Encode.Value
    , decodeResponse : Decode.Decoder res
    , porterMsg : Msg req res msg -> msg
    }


{-| Initialize model.
-}
init : Model req res msg
init =
    Model
        { handlers = Dict.empty
        , nextId = 0
        }


type alias ResponseHandlerMap req res msg =
    Dict.Dict MsgId (FullRequest req res msg)


encode : (req -> Encode.Value) -> MsgId -> req -> Encode.Value
encode encodeReq id msg =
    Encode.object
        [ ( "id", Encode.int id )
        , ( "msg", encodeReq msg )
        ]


{-| Module messages.
-}
type Msg req res msg
    = SendWithNextId (FullRequest req res msg)
    | Receive Encode.Value

{-| Internal type used by requests that have a response handler.
-}
type FullRequest req res msg = FullRequest req (List (res -> Request req res)) (res -> msg)

{-| Opaque type of a 'request'. Use the `request` function to create one,
chain them using `andThen` and finally send it using `sendRequest`.
-}
type Request req res = Request req (List (res -> Request req res))


{-| Subscribe to messages from ports.
-}
subscriptions : Config req res msg -> Sub msg
subscriptions config =
    config.incomingPort Receive
        |> Sub.map config.porterMsg


{-| Initiate a message send.
-}
send : Config req res msg -> (res -> msg) -> req -> Cmd msg
send config responseHandler request =
    runSendRequest config (FullRequest request [] responseHandler)


{-| Starts a request that can be sent at a later time using `sendRequest`,
    and that can be combined using `andThen`.
-}
request : req -> Request req res
request req = Request req []


{-| Chains two Porter requests together:
    Run a second one right away when the first returns using its result in the request.
-}
andThen : (res -> Request req res) -> Request req res -> Request req res
andThen reqfun (Request initial_req reqfuns) = Request initial_req (reqfun :: reqfuns)

{-| Transforms a request

-}
map : (res -> req) -> Request req res -> Request req res
map func reqA =
    reqA
        |> andThen (\a -> request (func a))

{-| Run a request using the results of two earlier requests.
 -}
map2 : (res -> res -> req) -> Request req res -> Request req res -> Request req res
map2 func reqA reqB =
  reqA
      |> andThen (\a -> reqB
      |> andThen (\b -> request (func a b)))

{-|-}
map3 : (res -> res -> res -> req) -> Request req res -> Request req res -> Request req res -> Request req res
map3 func reqA reqB reqC =
  reqA
    |> andThen (\a -> reqB
    |> andThen (\b -> reqC
    |> andThen (\c -> request (func a b c))))

{-| Sends a request earlier started using `request`.
-}
sendRequest : Config req res msg -> (res -> msg) -> (Request req res) -> Cmd msg
sendRequest config response_handler (Request req reqfuns)  =
    runSendRequest config (FullRequest req (List.reverse reqfuns) response_handler)


{-| Internal function that performs the specified request as a command.
 -}
runSendRequest : Config req res msg -> FullRequest req res msg -> Cmd msg
runSendRequest config request =
    SendWithNextId request
        |> Task.succeed
        |> Task.perform identity
        |> Cmd.map config.porterMsg


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
        SendWithNextId (request) ->
            let
                id =
                    safeId model.nextId model.handlers
                extractMsg (FullRequest message _ _ ) = message
                msg = extractMsg request
            in
                ( Model
                    -- We remember the next ID we ought to propose, though we
                    -- will check it then before using it.
                    { handlers = Dict.insert id request model.handlers
                    , nextId = id + 1
                    }
                , config.outgoingPort (encode config.encodeRequest id msg)
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

{-| Internal function that chains the steps of a FullRequest after one another.
-}
handleResponse : Config req res msg -> Model req res msg -> MsgId -> res -> FullRequest req res msg -> (Model req res msg, Cmd msg)
handleResponse config (Model model) id res (FullRequest msg mappers finalResponseHandler) =
    case mappers of
        [] ->
          ( Model { model | handlers = Dict.remove id model.handlers }
          , Task.perform finalResponseHandler (Task.succeed res)
          )
        (mapper :: mappers) ->
            let
                request =
                    mapper res
                extractMsg (Request msg _) = msg
                extractMappers (Request _ req_mappers) = req_mappers
            in
          ( Model { model | handlers = Dict.remove id model.handlers }
          , runSendRequest config (FullRequest (extractMsg request) ((extractMappers request) ++ mappers) finalResponseHandler)
          )
