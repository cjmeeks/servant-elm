module GetBooksSource exposing (..)

import Http
import Json.Decode exposing (..)


getBooks : Bool -> (Maybe String) -> (Maybe Int) -> String -> (List (Maybe Bool)) -> Http.Request (List Book)
getBooks query_published query_sort query_year query_category query_filters =
    let
        params =
            List.filter (not << String.isEmpty)
                [ if query_published then
                    "query_published="
                  else
                    ""
                , query_sort
                    |> Maybe.map (Url.percentEncode >> (++) "sort=")
                    |> Maybe.withDefault ""
                , query_year
                    |> Maybe.map (toString >> Url.percentEncode >> (++) "year=")
                    |> Maybe.withDefault ""
                , Just query_category
                    |> Maybe.map (Url.percentEncode >> (++) "category=")
                    |> Maybe.withDefault ""
                , query_filters
                    |> List.map (\val -> "query_filters[]=" ++ (val |> toString |> Url.percentEncode))
                    |> String.join "&"
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                String.join "/"
                    [ ""
                    , "books"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson <| Json.Decode.list (jsonDecBook)
            , timeout =
                Nothing
            , withCredentials =
                False
            }
