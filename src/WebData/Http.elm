module WebData.Http
    exposing
        ( get
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
        )

{-| Friendly abstraction over remote API communication in JSON.

# Commands
@docs get, getWithCache, post, put, patch, delete

# Tasks
@docs getTask, getWithCacheTask, postTask, putTask, patchTask, deleteTask

# Helpers
@docs url
-}

import Http exposing (Header, Error, Response)
import Task exposing (Task)
import Json.Decode exposing (Decoder, Value)
import WebData exposing (WebData)


noCache : Header
noCache =
    Http.header "Cache-Control" "no-store, must-revalidate, no-cache, max-age=0"


{-| Convert an apiCall `Task` to a `Cmd msg` with the help of a
tagger function (`WebData success -> msg`).
-}
toCmd : (WebData success -> msg) -> Http.Request success -> Cmd msg
toCmd tagger =
    Http.send (tagger << WebData.fromResult)


request :
    String
    -> List Header
    -> String
    -> Decoder success
    -> Http.Body
    -> Http.Request success
request method headers url successDecoder body =
    Http.request
        { method = method
        , headers = headers
        , url = url
        , body = body
        , expect = Http.expectJson successDecoder
        , timeout = Nothing
        , withCredentials = False
        }


getRequest : List Header -> String -> Decoder success -> Http.Request success
getRequest headers url decoder =
    request "GET" headers url decoder Http.emptyBody


{-| `GET` request as a task.
Has a `no-cache` header to ensure data integrity.
-}
getTask : String -> Decoder success -> Task Error success
getTask url decoder =
    getRequest [ noCache ] url decoder
        |> Http.toTask


{-| `GET` request as a command.
Has a `no-cache` header to ensure data integrity.
-}
get : String -> (WebData success -> msg) -> Decoder success -> Cmd msg
get url tagger decoder =
    getRequest [ noCache ] url decoder
        |> toCmd tagger


{-| `GET` request as a task, with cache. *NB.* allowing cache in API `GET` calls can lead
to strange conditions.
-}
getWithCacheTask : String -> Decoder success -> Task Error success
getWithCacheTask url decoder =
    getRequest [] url decoder
        |> Http.toTask


{-| `GET` request as a command, with cache. *NB.* allowing cache in API `GET` calls can lead
to strange conditions.
-}
getWithCache : String -> (WebData success -> msg) -> Decoder success -> Cmd msg
getWithCache url tagger decoder =
    getRequest [] url decoder
        |> toCmd tagger


{-| `POST` request as a task.
-}
postTask :
    String
    -> Decoder success
    -> Json.Decode.Value
    -> Task Error success
postTask url decoder body =
    request "POST" [] url decoder (Http.jsonBody body)
        |> Http.toTask


{-| `POST` request as a command.
-}
post :
    String
    -> (WebData success -> msg)
    -> Decoder success
    -> Json.Decode.Value
    -> Cmd msg
post url tagger decoder body =
    request "POST" [] url decoder (Http.jsonBody body)
        |> toCmd tagger


{-| `PUT` request as a task.
-}
putTask :
    String
    -> Decoder success
    -> Json.Decode.Value
    -> Task Error success
putTask url decoder body =
    request "PUT" [] url decoder (Http.jsonBody body)
        |> Http.toTask


{-| `PUT` request as a command.
-}
put :
    String
    -> (WebData success -> msg)
    -> Decoder success
    -> Json.Decode.Value
    -> Cmd msg
put url tagger decoder body =
    request "PUT" [] url decoder (Http.jsonBody body)
        |> toCmd tagger


{-| `PATCH` request as a task.
-}
patchTask :
    String
    -> Decoder success
    -> Json.Decode.Value
    -> Task Error success
patchTask url decoder body =
    request "PATCH" [] url decoder (Http.jsonBody body)
        |> Http.toTask


{-| `PATCH` request as a command.
-}
patch :
    String
    -> (WebData success -> msg)
    -> Decoder success
    -> Json.Decode.Value
    -> Cmd msg
patch url tagger decoder body =
    request "PATCH" [] url decoder (Http.jsonBody body)
        |> toCmd tagger


{-| `DELETE` request as a task.
-}
deleteTask :
    String
    -> Decoder success
    -> Json.Decode.Value
    -> Task Error success
deleteTask url decoder body =
    request "DELETE" [] url decoder (Http.jsonBody body)
        |> Http.toTask


{-| `DELETE` request as a command.
-}
delete :
    String
    -> (WebData success -> msg)
    -> Decoder success
    -> Json.Decode.Value
    -> Cmd msg
delete url tagger decoder body =
    request "DELETE" [] url decoder (Http.jsonBody body)
        |> toCmd tagger


{-| This is the old `url` function from evancz/elm-http.

Create a properly encoded URL with a [query string][qs]. The first argument is
the portion of the URL before the query string, which is assumed to be
properly encoded already. The second argument is a list of all the
key/value pairs needed for the query string. Both the keys and values
will be appropriately encoded, so they can contain spaces, ampersands, etc.
[qs]: http://en.wikipedia.org/wiki/Query_string
    url "http://example.com/users" [ ("name", "john doe"), ("age", "30") ]
    -- http://example.com/users?name=john+doe&age=30
-}
url : String -> List ( String, String ) -> String
url baseUrl args =
    case args of
        [] ->
            baseUrl

        _ ->
            baseUrl ++ "?" ++ String.join "&" (List.map queryPair args)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    queryEscape key ++ "=" ++ queryEscape value


queryEscape : String -> String
queryEscape string =
    String.join "+" (String.split "%20" (Http.encodeUri string))
