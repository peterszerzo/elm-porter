module Porter.Multi
    exposing
        ( Request
        , request
        , fromSimple
        , andThen
        , andThenResult
        , map
        , map2
        , map3
        , send
        )

{-| With `Porter.Multi` you can create requests that have specialized return values.

This is an advanced usage pattern of Porter. For most use-cases, using Porter itself should be sufficient. If, however, you have a tangible issue with expressing chaining and transformations with your port messages, read along :).

The setup is exactly the same as when using Porter normally. Just use the `request` and `send` commands that this module provides instead to create requests with specialized return types.

Mapping over responses and chaining requests is supported.


# Send Messages

@docs Request
@docs request, send
@docs fromSimple


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


{-| Creates a new Porter.Multi request, specifying:

  - the request itself in the common `req` type
  - A response handler function tha turns the common `res` type into a specialized `a`.

-}
request : req -> (res -> a) -> Request req res a
request req responseHandler =
    SimpleRequest (Porter.request req) responseHandler


{-| Turns a simple Porter request into a Porter.Multi request
-}
fromSimple : Porter.Request req res -> Request req res res
fromSimple request =
    SimpleRequest request identity


{-| Combines together multiple Porter.Multi requests
-}
andThen : (a -> Request req res b) -> Request req res a -> Request req res b
andThen reqfun req =
    case req of
        SimpleRequest porterReq requestMapper ->
            ComplexRequest (porterReq) (requestMapper >> reqfun)

        ComplexRequest porterReq nextRequestFun ->
            ComplexRequest porterReq (\res -> andThen reqfun (nextRequestFun res))

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

        SimpleRequest porterReq requestMapper ->
            ComplexRequest (porterReq)
                (requestMapper >> Result.Extra.unpack (ShortCircuit << Err) (reqfun))

        ComplexRequest porterReq nextRequestFun ->
            ComplexRequest porterReq (\res -> andThenResult reqfun (nextRequestFun res))


{-| Turns the request's specialized response type into a different type.
-}
map : (a -> b) -> Request req res a -> Request req res b
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
send : Config req res msg -> (a -> msg) -> Request req res a -> Cmd msg
send config msgHandler request =
    Porter.Internals.multiSend config msgHandler request
