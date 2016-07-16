module WebData.Http
    exposing
        ( WebDataCmd
        , BodylessWebDataCmd
        , WebDataTask
        , BodylessWebDataTask
        , get
        , getWithCache
        , post
        , put
        , patch
        , delete
        , getTask
        , getWithCacheTask
        , postTask
        , putTask
        , patchTask
        , deleteTask
        , url
        , asWebDataTask
        , toCmd
        )

{-| Friendly abstraction over remote API communication in JSON.

# Commands
@docs WebDataCmd, BodylessWebDataCmd
@docs get, getWithCache, post, put, patch, delete

# Tasks
@docs WebDataTask, BodylessWebDataTask
@docs getTask, getWithCacheTask, postTask, putTask, patchTask, deleteTask

# Helpers
@docs url, asWebDataTask, toCmd
-}

import Http
import HttpBuilder exposing (..)
import Task exposing (Task)
import Json.Decode exposing (Decoder, Value)
import WebData exposing (WebData)


never : Never -> a
never n =
    never n


{-| Typical signature for the `methodTask` functions. Takes a success decoder,
a JSON value for body and the url, returning a task.
-}
type alias WebDataTask success =
    Decoder success -> Value -> String -> Task Never (WebData success)


{-| Signature for a `methodTask` function without an HTTP body (namely, `getTask`).
Takes a success decoder, and the url, returning a task.
-}
type alias BodylessWebDataTask success =
    Decoder success -> String -> Task Never (WebData success)


{-| Signature for the `method` functions. Takes a "tagger", that can turn
a `WebData success` into a component specific message, along with the `WebDataTask`
arguments, returning a command.
-}
type alias WebDataCmd success msg =
    (WebData success -> msg) -> Decoder success -> Value -> String -> Cmd msg


{-| Signature for a `method` function without an HTTP body (namely, `get`).
Takes a "tagger", that can turn a `WebData success` into a component specific message,
along with the `BodylessWebDataTask` arguments, returning a command.
-}
type alias BodylessWebDataCmd success msg =
    (WebData success -> msg) -> Decoder success -> String -> Cmd msg


{-| Convert a `Task` produced by HttpBuilder, to a `Task Never (WebData s)`.
-}
asWebDataTask : Task (Error String) (Response success) -> Task Never (WebData success)
asWebDataTask =
    Task.map .data >> Task.toResult >> Task.map WebData.fromResult


{-| Convert an apiCall `Task` to a `Cmd msg` with the help of a
tagger function (`WebData success -> msg`).
-}
toCmd : (WebData success -> msg) -> Task Never (WebData success) -> Cmd msg
toCmd =
    Task.perform never


{-| Not to be exposed: a helper for turning a WebDataTask into a WebDataCmd.
-}
taskFnToCmdFn : WebDataTask success -> WebDataCmd success msg
taskFnToCmdFn taskFn tagger decoder body url =
    taskFn decoder body url
        |> toCmd tagger


type HeaderSetting
    = NoCache
    | JustJson


headers : HeaderSetting -> RequestBuilder -> RequestBuilder
headers cacheSetting =
    case cacheSetting of
        NoCache ->
            withHeaders
                [ ( "content-type", "application/json" )
                , ( "Cache-Control", "no-store, must-revalidate, no-cache, max-age=0" )
                ]

        _ ->
            withHeader "content-type" "application/json"


{-| `GET` request as a task.
Has a `no-cache` header to ensure data integrity.
-}
getTask : BodylessWebDataTask success
getTask successDecoder url =
    url
        |> HttpBuilder.get
        |> headers NoCache
        |> send (jsonReader successDecoder) stringReader
        |> asWebDataTask


{-| `GET` request as a command.
Has a `no-cache` header to ensure data integrity.
-}
get : BodylessWebDataCmd success msg
get tagger decoder url =
    getTask decoder url
        |> toCmd tagger


{-| `GET` request as a task, with cache. *NB.* allowing cache in API `GET` calls can lead
to strange conditions.
-}
getWithCacheTask : BodylessWebDataTask success
getWithCacheTask successDecoder url =
    url
        |> HttpBuilder.get
        |> headers JustJson
        |> send (jsonReader successDecoder) stringReader
        |> asWebDataTask


{-| `GET` request as a command, with cache. *NB.* allowing cache in API `GET` calls can lead
to strange conditions.
-}
getWithCache : BodylessWebDataCmd success msg
getWithCache tagger decoder url =
    getWithCacheTask decoder url
        |> toCmd tagger


{-| `POST` request as a task.
-}
postTask : WebDataTask success
postTask successDecoder body url =
    url
        |> HttpBuilder.post
        |> headers JustJson
        |> withJsonBody body
        |> send (jsonReader successDecoder) stringReader
        |> asWebDataTask


{-| `POST` request as a command.
-}
post : WebDataCmd success msg
post =
    taskFnToCmdFn postTask


{-| `PUT` request as a task.
-}
putTask : WebDataTask success
putTask successDecoder body url =
    url
        |> HttpBuilder.put
        |> headers JustJson
        |> withJsonBody body
        |> send (jsonReader successDecoder) stringReader
        |> asWebDataTask


{-| `PUT` request as a command.
-}
put : WebDataCmd success msg
put =
    taskFnToCmdFn putTask


{-| `PATCH` request as a task.
-}
patchTask : WebDataTask success
patchTask successDecoder body url =
    url
        |> HttpBuilder.patch
        |> headers JustJson
        |> withJsonBody body
        |> send (jsonReader successDecoder) stringReader
        |> asWebDataTask


{-| `PATCH` request as a command.
-}
patch : WebDataCmd success msg
patch =
    taskFnToCmdFn patchTask


{-| `DELETE` request as a task.
-}
deleteTask : WebDataTask success
deleteTask successDecoder body url =
    url
        |> HttpBuilder.delete
        |> headers JustJson
        |> withJsonBody body
        |> send (jsonReader successDecoder) stringReader
        |> asWebDataTask


{-| `DELETE` request as a command.
-}
delete : WebDataCmd success msg
delete =
    taskFnToCmdFn deleteTask


{-| Same as the elm-http `url` function, re-exposed for convenience.
See documentation for
[`Http.url`](http://package.elm-lang.org/packages/evancz/elm-http/latest/Http#url).
-}
url : String -> List ( String, String ) -> String
url =
    Http.url
