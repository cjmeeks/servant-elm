module GetWithCookieSource exposing (..)

import Http
import Json.Decode exposing (..)


getWithcookie : Http.Request Int
getWithcookie =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "with-cookie"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson <| Json.Decode.int
        , timeout =
            Nothing
        , withCredentials =
            False
        }
