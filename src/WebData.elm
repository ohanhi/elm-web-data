module WebData exposing (WebData(..), fromResult)

{-| Type representing remote API data.

@docs WebData, fromResult
-}

import Http


{-| Type representing remote API data. A `Success` value contains the type that you
provide a decoder for, while the `Failure` value contains an `HttpBuilder.Error String`.
Read more about the [HttpBuilder errors](http://package.elm-lang.org/packages/lukewestby/elm-http-builder/2.0.0/HttpBuilder#Error).

This idea is originally from the Kris Jenkins'
[RemoteData](http://package.elm-lang.org/packages/krisajenkins/elm-exts/25.8.0/Exts-RemoteData)
pattern.
-}
type WebData a
    = NotAsked
    | Loading
    | Failure Http.Error
    | Success a


{-| Convert a `WebData` state into a `Maybe`. This can be useful when multiple
pieces of `WebData` are needed to compose a view.
-}
toMaybe : WebData a -> Maybe a
toMaybe state =
    case state of
        Success a ->
            Just a

        _ ->
            Nothing


{-| Convert a `Result Error` produced by HttpBuilder, to a `WebData` value.
-}
fromResult : Result Http.Error success -> WebData success
fromResult result =
    case result of
        Err e ->
            Failure e

        Ok x ->
            Success x
