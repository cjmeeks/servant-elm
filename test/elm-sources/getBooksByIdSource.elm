module GetBooksByIdSource exposing (..)

import Http


getBooksById : Int -> Http.Request Book
getBooksById capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_id |> toString |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson <| jsonDecBook
        , timeout =
            Nothing
        , withCredentials =
            False
        }
