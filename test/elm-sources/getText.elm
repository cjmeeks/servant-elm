module getText exposing (..)

import Http
import Json.Decode exposing (..)


getText : Http.Request (String)
getText =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "text"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectString
        , timeout =
            Nothing
        , withCredentials =
            False
        }
