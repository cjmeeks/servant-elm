module GetWithACookieSource exposing (..)

import Http
import Json.Decode exposing (..)


getWithacookie : Http.Request String
getWithacookie =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "with-a-cookie"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson <| Json.Decode.string
        , timeout =
            Nothing
        , withCredentials =
            False
        }
