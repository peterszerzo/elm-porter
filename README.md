# elm-porter

Port message manager that emulate a request-response style communication, a'la `Http.send Response request`.

## Example

Run the example like so:

`cd ./example`

`elm-reactor`

Navigate to: `http://localhost:8000/index.html`

## How it works

```elm
port module Example exposing (..)

import Porter
import Html exposing (Html, text)
import Json.Encode as Encode
import Json.Decode as Decode


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
    }


init : ( Model, Cmd Msg )
init =
    ( { porter = Porter.init
      , response = ""
      }
      -- Send a request through porter, specifying the response handler directly
      , Porter.send porterConfig Receive (Porter.request "Reverse me!")
    )



-- Message includes Porter's message


type Msg
    = PorterMsg (Porter.Msg String String Msg)
    | Receive String



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



-- Set up porter's subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Porter.subscriptions porterConfig



-- Any view you like


view : Model -> Html Msg
view model =
    text (toString model)



--


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
```

## Handling messages in JS

Outgoing messages into JavaScript as JSON, with an id that Porter generates and uses to match up with response handlers.

```js
const app = Elm.Example.fullscreen()
app.ports.outgoing.subscribe(msgWithId => {
  const id = msgWithId.id
  const request = msgWithId.msg
  // Reverse response
  const response = request.split("").reverse().join("")
  // Simply include the same `id` and the response under the `msg` key.
  app.ports.incoming.send({
    id: id,
    msg: response
  })
})
```

## Chaining Requests

If you want to perform multiple requests where some of these request depend on responses from other requests, you can use `Porter.request` in combination with `Porter.andThen` and `Porter.sendRequest`.

```elm
Porter.request ("Reverse me too!")
  |> Porter.andThen (\reversedStr -> Porter.request (reversedStr ++ " The Quick Brown Fox!"))
  |> Porter.andThen (\reversedStr -> Porter.request (reversedStr ++ " A man a plan a canal: panama"))
  |> Porter.sendRequest porterConfig Receive
```


## Changelog

###  2.0

- The `Porter.Config` type now has a `porterMsg`-field.
- Signature of `Porter.send` was changed: 
  - It now takes the `porterConfig` as argument, meaning (in combination with the previous change) that `Cmd.map`ping the result to your Msg type is no longer necessary because this is handled for you.
  - Requests are now constructed using `Porter.request` and can be chained using `Porter.andThen` before passing them off to `Porter.send`. 

So: Where in Version 1 you'd use `Porter.send responseHandler request`, you'd now use `Porter.send porterConfig responseHandler (Porter.request request)`.


### 1.0

First stable release
