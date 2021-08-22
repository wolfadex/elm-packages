module Api exposing (docsUrl, elmJsonUrl, readmeUrl, releasesUrl, routes, searchUrl)

import ApiRoute
import DataSource exposing (DataSource)
import Html exposing (Html)
import Route exposing (Route)


routes :
    DataSource (List Route)
    -> (Html Never -> String)
    -> List (ApiRoute.ApiRoute ApiRoute.Response)
routes getStaticRoutes htmlToString =
    []


searchUrl : String
searchUrl =
    baseUrl ++ "/search.json"


releasesUrl : String -> String -> String
releasesUrl author project =
    baseUrl ++ "/packages/" ++ author ++ "/" ++ project ++ "/releases.json"


elmJsonUrl : String -> String -> String
elmJsonUrl author version =
    baseUrl ++ "/packages/" ++ author ++ "/" ++ version ++ "/elm.json"


docsUrl : String -> String -> String -> String
docsUrl author project version =
    baseUrl ++ "/packages/" ++ author ++ "/" ++ project ++ "/" ++ version ++ "/docs.json"


readmeUrl : String -> String -> String -> String
readmeUrl author project version =
    baseUrl ++ "/packages/" ++ author ++ "/" ++ project ++ "/" ++ version ++ "/README.md"


baseUrl : String
baseUrl =
    "https://package.elm-lang.org"
