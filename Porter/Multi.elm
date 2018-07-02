module Porter.Multi
    exposing
        ( Request
        , request
        , andThen
        , andThenResult
        , map
        , map2
        , map3
        , send
        )

{-| With `Porter.Multi` you can create requests that have specialized return values.
Use this if the normal response types are not good enough for you.

The setup is exactly the same as when using Porter normally; and just use the `request` and `send` commands that this module provides instead to create requests with specialized return types.

Mapping over responses and chaining requests is supported.


# Send Messages

@docs Request
@docs request, send


# Chain Requests

@docs andThen, andThenResult, map, map2, map3


# Low-level Stuff

@docs configToPorterConfig

-}

import Porter exposing (Model, Config)
import Porter.Internals exposing (MultiRequest(..), Msg(..))
import Result.Extra


{-| We can either:

  - Perform a single request, which will run the Porter Request and at the end turn the result into an `a`,
  - Perform multiple requests: At the end of this request, run a function on the output that generates the next request. Note that this is set up in such a way that only the type of the final request in the chain matters.
  - Short-circuit; just pass on the value `a` without doing any more requests.

-}
type alias Request req res a =
    Porter.Internals.MultiRequest req res a



-- type Request req res a
-- = SimpleRequest (Porter.Request req res) (res -> a)
-- | ComplexRequest (Porter.Request req res) (res -> Request req res a)
-- | ShortCircuit a
-- | To configure Porter.Multi, you'll need:
-- 1.  An outgoingPort of the correct type
-- 2.  An incoming port of the correct type
-- 3.  A way to encode the request type into JSON
-- 4.  A way to decode the response JSON into an intermediate common 'res' type.
-- 5.  A `msg` that will be used to perform the internal chaining of requests.
-- type alias Config req res msg =
--     { outgoingPort : Encode.Value -> Cmd msg
--     , incomingPort : (Encode.Value -> Porter.Msg req res msg) -> Sub (Porter.Msg req res msg)
--     , encodeRequest : req -> Encode.Value
--     , decodeResponse : Decode.Decoder res
--     , porterMultiMsg : Msg req res msg -> msg
--     }
-- {-| Allows us to perform low-level Porter calls using the Porter.Multi configuration
-- Unless you know what you are doing, you probably don't need this ;-)
-- -}
-- configToPorterConfig : Config req res msg -> Porter.Config req res msg
-- configToPorterConfig config =
--     { porterMsg = (\msg -> (config.porterMultiMsg (PorterMsg msg)))
--     , outgoingPort = config.outgoingPort
--     , incomingPort = config.incomingPort
--     , encodeRequest = config.encodeRequest
--     , decodeResponse = config.decodeResponse
--     }
-- | The internal message type. Eiher:
--   - A low level Porter message
--   - One step in a multi-step request chain.
-- type alias Msg req res msg = Porter.Msg req res msg
-- type Msg req res msg
--     = PorterMsg (Porter.Msg req res msg)
--     | ResolveChain (Request req res msg)
-- | The Porter.Multi Model is used to keep track of state
-- across one or multiple port request<->response steps.
-- type alias Model req res msg = Porter.Model req res msg
-- type alias Model req res msg =
--     { porter_model : Porter.Model req res msg }
-- | Initializes a new Porter.Multi model.
-- Should probably be called as part of your program's `init` call.
-- init : Model req res msg
-- init =
--     { porter_model = Porter.init }


{-| Creates a new Porter.Multi request, specifying:

  - the request itself in the common `req` type
  - A response handler function tha turns the common `res` type into a specialized `a`.

-}
request : req -> (res -> a) -> Request req res a
request req response_handler =
    SimpleRequest (Porter.request req) response_handler


{-| Combines together multiple Porter.Multi requests
-}
andThen : (a -> Request req res b) -> Request req res a -> Request req res b
andThen reqfun req =
    case req of
        SimpleRequest porter_req request_mapper ->
            ComplexRequest (porter_req) (request_mapper >> reqfun)

        ComplexRequest porter_req next_request_fun ->
            ComplexRequest porter_req (\res -> andThen reqfun (next_request_fun res))

        ShortCircuit val ->
            reqfun val


{-| andThenResult function similar to `andThen`,
but will short-circuit the request/response chain once an `Err val` is received.

Working with Result-objects is very common, since the specialising of `res -> a` (that you pass to `request`) is usually an operation that might fail (like a Decoder)

-}
andThenResult : (a -> Request req res (Result err b)) -> Request req res (Result err a) -> Request req res (Result err b)
andThenResult reqfun req =
    case req of
        ShortCircuit (Err val) ->
            ShortCircuit (Err val)

        ShortCircuit (Ok val) ->
            (reqfun val)

        SimpleRequest porter_req request_mapper ->
            ComplexRequest (porter_req)
                (request_mapper >> Result.Extra.unpack (ShortCircuit << Err) (reqfun))

        ComplexRequest porter_req next_request_fun ->
            ComplexRequest porter_req (\res -> andThenResult reqfun (next_request_fun res))


{-| Turns the request's specialized response type into a different type.
-}
map : (a -> b) -> Request req res a -> Request req res b
map mapfun req =
    Porter.Internals.multiMap mapfun req


{-| Chains two requests, and then combines their two responses into one new `c`.
-}
map2 : (a -> b -> c) -> Request req res a -> Request req res b -> Request req res c
map2 mapfun req_a req_b =
    req_a
        |> andThen (\res_a -> req_b |> map (mapfun res_a))


{-| Chains three requests, and then combines their three responses into one new `c`.
-}
map3 : (a -> b -> c -> d) -> Request req res a -> Request req res b -> Request req res c -> Request req res d
map3 mapfun req_a req_b req_c =
    req_a
        |> andThen (\res_a -> map2 (mapfun res_a) req_b req_c)


{-| Actually sends a (chain of) request(s).

A final handler needs to be specified that turns the final result into a `msg`.
This `msg` will be called with the final resulting `a` once the final response has returned.

-}
send : Config req res msg -> (a -> msg) -> Request req res a -> Cmd msg
send config msg_handler request =
    Porter.Internals.multiSend config msg_handler request
