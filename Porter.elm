module Porter exposing (Model, Config, Msg, subscriptions, init, update, send)

{-| Port message manager to emulate a request-response style communication through ports, a'la `Http.send ResponseHandler request`.


# Configuration

@docs Config


# The setup

@docs Model, Msg, init, update, subscriptions


# Send messages

@docs send

-}

import Dict
import Task
import Random
import Json.Encode as Encode
import Json.Decode as Decode


type alias MsgId =
    Int


{-| Porter model, used to keep track of response handlers.
-}
type Model req res msg
    = Model (ResponseHandlerMap res msg)


{-| Porter configuration, containing ports and message encoders/decoders.
-}
type alias Config req res msg =
    { outgoingPort : Encode.Value -> Cmd msg
    , incomingPort : (Encode.Value -> Msg req res msg) -> Sub (Msg req res msg)
    , encodeRequest : req -> Encode.Value
    , decodeResponse : Decode.Decoder res
    }


{-| Initialize model.
-}
init : Model req res msg
init =
    Model Dict.empty


type alias ResponseHandlerMap res msg =
    Dict.Dict MsgId (res -> msg)


encode : (req -> Encode.Value) -> MsgId -> req -> Encode.Value
encode encodeReq id msg =
    Encode.object
        [ ( "id", Encode.int id )
        , ( "msg", encodeReq msg )
        ]


{-| Module messages.
-}
type Msg req res msg
    = NewRandomId (res -> msg) req MsgId
    | Receive Encode.Value


{-| Subscribe to messages from ports.
-}
subscriptions : Config req res msg -> Sub (Msg req res msg)
subscriptions config =
    config.incomingPort Receive


{-| Initiate a message send.
-}
send : (res -> msg) -> req -> Cmd (Msg req res msg)
send responseHandler msg =
    Random.generate
        (NewRandomId responseHandler msg)
        (Random.int 0 100000)


{-| Update port model based on local messages. Returns a new port model and a local command that must be mapped to the parent message.
-}
update : Config req res msg -> Msg req res msg -> Model req res msg -> ( Model req res msg, Cmd msg )
update config msg (Model model) =
    case msg of
        NewRandomId responseHandler msg id ->
            ( Model (Dict.insert id responseHandler model)
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
                        Dict.get id model
                            |> Maybe.map
                                (\handleResponse ->
                                    ( Model
                                        (Dict.remove id model)
                                    , Task.perform handleResponse (Task.succeed res)
                                    )
                                )
                            |> Maybe.withDefault ( Model model, Cmd.none )
                    )
                |> Result.withDefault ( Model model, Cmd.none )
