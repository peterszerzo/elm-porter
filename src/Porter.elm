module Porter exposing
    ( Config
    , Model, Msg, init, update, subscriptions
    , map, map2, map3, andThen
    , Request
    , request, simpleRequest, succeed, send
    )

{-| Port message manager to emulate a request-response style communication through ports, a'la `Http.send ResponseHandler request`.


# Configuration

@docs Config


# The setup

@docs Model, Msg, init, update, subscriptions


# Transform messages

@docs map, map2, map3, andThen


# Send messages

@docs Request
@docs request, simpleRequest, succeed, send

-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Porter.Internals exposing (Msg(..), MultiRequest(..), Request(..), RequestWithHandler(..), unpackResult)
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


{-| Initialize model
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


{-| Opaque type for porter requests, where we can either:

  - perform a single request, which will run the Porter Request and at the end turn the result into an `outputRes`. See [request](#request).
  - perform multiple requests: At the end of this request, run a function on the output that generates the next request. Note that this is set up in such a way that only the type of the final request in the chain matters. See [andThen](#andThen).
  - short-circuit: just pass on the value `outputRes` without doing any more requests. See [succeed](#succeed).

-}
type alias Request req res outputRes =
    Porter.Internals.MultiRequest req res outputRes


{-| A simpler version of [request](#request) where the end result is not converted into a specialized type, but left the same.
-}
simpleRequest : req -> Request req res res
simpleRequest =
    request identity


{-| Creates a new request, specifying:

  - the request itself in the common `req` type
  - a response handler function tha turns the common `res` type into a specialized `a`.

-}
request : (res -> outputRes) -> req -> Request req res outputRes
request responseHandler req =
    SimpleRequest (Request req []) responseHandler


{-| Succeed with an `outputRes` value without making an actual request. This is especially useful inside [andThen](#andThen), where the request chain may need to shortcircuit because of the return value (e.g. should not update a record that hasn't been found).
-}
succeed : outputRes -> Request req res outputRes
succeed =
    ShortCircuit


{-| Combines together multiple Porter.Multi requests
-}
andThen : (outputResA -> Request req res outputResB) -> Request req res outputResA -> Request req res outputResB
andThen reqfun req =
    case req of
        SimpleRequest porterReq requestMapper ->
            ComplexRequest porterReq (requestMapper >> reqfun)

        ComplexRequest porterReq nextRequestFun ->
            ComplexRequest porterReq (\res -> andThen reqfun (nextRequestFun res))

        ShortCircuit val ->
            reqfun val


{-| Turns the request's specialized response type into a different type.
-}
map : (outputResA -> outputResB) -> Request req res outputResA -> Request req res outputResB
map mapfun req =
    Porter.Internals.multiMap mapfun req


{-| Chains two requests, and then combines their two responses into one new `c`.
-}
map2 : (a -> b -> c) -> Request req res a -> Request req res b -> Request req res c
map2 mapfun reqA reqB =
    reqA
        |> andThen (\resA -> reqB |> map (mapfun resA))


{-| Chains three requests, and then combines their three responses into one new `c`.
-}
map3 : (a -> b -> c -> d) -> Request req res a -> Request req res b -> Request req res c -> Request req res d
map3 mapfun reqA reqB reqC =
    reqA
        |> andThen (\resA -> map2 (mapfun resA) reqB reqC)


{-| Actually sends a (chain of) request(s).

A final handler needs to be specified that turns the final result into a `msg`.
This `msg` will be called with the final resulting `a` once the final response has returned.

-}
send : Config req res msg -> (outputRes -> msg) -> Request req res outputRes -> Cmd msg
send config msgHandler req =
    Porter.Internals.multiSend config msgHandler req


{-| Subscribe to messages from ports.
-}
subscriptions : Config req res msg -> Sub msg
subscriptions config =
    config.incomingPort Receive
        |> Sub.map config.porterMsg


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
handleResponse config (Model model) id res (RequestWithHandler req mappers finalResponseHandler) =
    case mappers of
        [] ->
            ( Model { model | handlers = Dict.remove id model.handlers }
            , Task.perform finalResponseHandler (Task.succeed res)
            )

        mapper :: mappersTail ->
            let
                mappedResponse =
                    mapper res

                newReq =
                    case mappedResponse of
                        Request unwrappedMsg _ ->
                            unwrappedMsg

                newMappers =
                    case mappedResponse of
                        Request _ reqMappers ->
                            reqMappers
            in
            ( Model { model | handlers = Dict.remove id model.handlers }
            , Porter.Internals.runSendRequest config
                (RequestWithHandler newReq
                    (newMappers ++ mappersTail)
                    finalResponseHandler
                )
            )
