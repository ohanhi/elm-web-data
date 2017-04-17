# Elm Web Data

DEPRECATED in favor of [`ohanhi/remotedata-http`](http://package.elm-lang.org/packages/ohanhi/remotedata-http/latest)

---

This package is intended as a quick and easy abstraction for dealing with backend data in Elm using the JSON format. As of now, there is very little configuration you can do.

If you like the pattern but need to for example set some more request headers, don't be afraid to simply copy the source code into your project and modify it there. I am willing to introduce some configuration though, so if you have ideas, please post them in the [GitHub Issues](https://github.com/ohanhi/elm-web-data/issues).

`WebData` is a type representing the state of some remote JSON data. It looks like this:

```elm
type WebData a
    = NotAsked
    | Loading
    | Failure (Error String)
    | Success a
```

Hats off to Kris Jenkins for coining this pattern in [elm-exts](http://package.elm-lang.org/packages/krisajenkins/elm-exts/25.8.0/Exts-RemoteData).


## Example usage

Say we have a component called `Cat`, which requests some auxiliary data from the backend.
Usually, we can define the command to do so just like this:


```elm
import WebData exposing (..)
import WebData.Http

type Msg
    = ReceiveCatData (WebData Cat)
-- ...

fetchData : Int -> Cmd Msg
fetchData catId =
    WebData.Http.get
      ("/api/cats/" ++ toString catId)
      catDecoder
      ReceiveCatData
```

Commands are usually what we need, but sometimes we need tasks for more fine-grained flow control. We can do that just as easily:

```elm
create : Cat -> Task Error (WebData Cat)
create catData =
    WebData.Http.postTask
      "/api/cats/"
      catDecoder
      (encode catData)
```

There is a function to make a command or a task for each of the HTTP verbs.

- Commands: `get`, `getWithCache`, `post`, `put`, `patch`, `delete`
- Tasks: `getTask`, `getWithCacheTask`, `postTask`, `putTask`, `patchTask`, `deleteTask`


# License

This is made by Ossi Hanhinen and licensed under [BSD (3-clause)](LICENSE).
