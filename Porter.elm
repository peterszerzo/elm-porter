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
    )

{-| Port message manager to emulate a request-response style communication through ports, a'la `Http.send ResponseHandler request`.


# Configuration

@docs Config


# The setup

@docs Model, Msg, init, update, subscriptions


# Send messages

@docs send

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
    Dict.Dict MsgId (Request req res msg)


encode : (req -> Encode.Value) -> MsgId -> req -> Encode.Value
encode encodeReq id msg =
    Encode.object
        [ ( "id", Encode.int id )
        , ( "msg", encodeReq msg )
        ]


{-| Module messages.
-}
type Msg req res msg
    = SendWithNextId (Request req res msg)
    | Receive Encode.Value

type Request req res msg = Request req (List (res -> req)) (res -> msg)
type PartialRequest req res = PartialRequest req (List (res -> req))

request : req -> PartialRequest req res
request req = PartialRequest req []

andThen : (res -> req) -> PartialRequest req res -> PartialRequest req res
andThen reqfun (PartialRequest initial_req reqfuns) = PartialRequest initial_req (reqfun :: reqfuns)

sendRequest : Config req res msg -> (res -> msg) -> (PartialRequest req res) -> Cmd msg
sendRequest config response_handler (PartialRequest req reqfuns)  =
    let
        request = Request req (List.reverse reqfuns) response_handler
    in
        SendWithNextId request
            |> Task.succeed
            |> Task.perform identity
            |> Cmd.map config.porterMsg


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
    SendWithNextId (Request request [] responseHandler)
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
                extractMsg (Request message _ _ ) = message
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
                            |> Maybe.map (handle_update config (Model model) id res)
                                -- (\(Request msg mappers handleResponse) ->
                                --      case mappers of
                                --          [] ->
                                --             ( Model { model | handlers = Dict.remove id model.handlers }
                                --             , Task.perform handleResponse (Task.succeed res)
                                --             )
                                --          (mapper :: mappers) ->
                                --             ( Model { model | handlers = Dict.remove id model.handlers }
                                --             , Task.perform SendWithNextId (Task.succeed (Request (mapper res) mappers handleResponse))
                                --             )
                                -- )
                            |> Maybe.withDefault ( Model model, Cmd.none )
                    )
                |> Result.withDefault ( Model model, Cmd.none )

handle_update : Config req res msg -> Model req res msg -> MsgId -> res -> Request req res msg -> (Model req res msg, Cmd msg)
handle_update config (Model model) id res (Request msg mappers handleResponse) =
    case mappers of
        [] ->
          ( Model { model | handlers = Dict.remove id model.handlers }
          , Task.perform handleResponse (Task.succeed res)
          )
        (mapper :: mappers) ->
          ( Model { model | handlers = Dict.remove id model.handlers }
          , Task.succeed (Request (mapper res) mappers handleResponse)
              |> Task.map SendWithNextId
              |> Task.perform config.porterMsg
          )
