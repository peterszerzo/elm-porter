port module Example exposing (main)

import Browser
import Html exposing (Html, li, p, text, ul)
import Json.Decode as Decode
import Json.Encode as Encode
import Porter


type alias Flags =
    Encode.Value



-- Configure Porter


port outgoing : Encode.Value -> Cmd msg


port incoming : (Decode.Value -> msg) -> Sub msg


porterConfig : Porter.Config String String Msg
porterConfig =
    { outgoingPort = outgoing
    , incomingPort = incoming

    -- Porter works with a single Request and Response data types. They can both be anything, as long as you supply decoders :)
    , encodeRequest = Encode.string
    , decodeResponse = Decode.string

    -- Porter uses a message added to your Msg type for its internal communications (See `type Msg` below)
    , porterMsg = PorterMsg
    }



-- Application model


type alias Model =
    { porter : Porter.Model String String Msg
    , response : String
    , advancedResponse : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { porter = Porter.init
      , response = ""
      , advancedResponse = ""
      }
    , Cmd.batch
        [ -- Send a request through porter, specifying the response handler directly
          Porter.send porterConfig Receive (Porter.request identity "Reverse me!")

        -- Or send multiple requests one after the other:
        , Porter.succeed "Reverse me too!"
            |> Porter.andThen (\reversedStr -> Porter.request identity (reversedStr ++ " The Quick Brown Fox!"))
            |> Porter.andThen (\reversedStr -> Porter.succeed <| reversedStr ++ "0")
            |> Porter.send porterConfig ReceiveAdvanced
        ]
    )



-- Message includes Porter's message


type Msg
    = PorterMsg (Porter.Msg String String Msg)
    | Receive String
    | ReceiveAdvanced String



-- Update porter accordingly


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PorterMsg porterMsg ->
            let
                ( porterModel, porterCmd ) =
                    Porter.update porterConfig porterMsg model.porter
            in
            ( { model | porter = porterModel }, porterCmd )

        Receive response ->
            ( { model | response = response }, Cmd.none )

        ReceiveAdvanced response ->
            ( { model | advancedResponse = response }, Cmd.none )



-- Set up porter's subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Porter.subscriptions porterConfig



-- Any view you like


view : Model -> Html Msg
view model =
    ul []
        [ li [] [ text model.response ]
        , li [] [ text model.advancedResponse ]
        ]



--


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
