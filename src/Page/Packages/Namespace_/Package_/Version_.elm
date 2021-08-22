module Page.Packages.Namespace_.Package_.Version_ exposing (Data, Model, Msg, page)

import Api
import DataSource exposing (DataSource)
import DataSource.Http
import Element exposing (..)
import Head
import Head.Seo as Seo
import Json.Encode
import Markdown.Parser
import Markdown.Renderer
import Maybe.Extra
import OptimizedDecoder exposing (Value)
import Package
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Secrets as Secrets
import Pages.Url
import Shared
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    { namespace : String, package : String, version : String }


page : Page RouteParams Data
page =
    Page.prerender
        { head = head
        , routes = routes
        , data = data
        }
        |> Page.buildNoState { view = view }


routes : DataSource (List RouteParams)
routes =
    DataSource.Http.get (Secrets.succeed Api.searchUrl)
        (OptimizedDecoder.list Package.decodeSummary
            |> OptimizedDecoder.map
                (List.filterMap
                    (\package ->
                        case String.split "/" package.name of
                            [ namespace, pkg ] ->
                                Just ( namespace, pkg )

                            _ ->
                                Nothing
                    )
                )
        )
        |> DataSource.andThen
            (List.map
                (\( author, project ) ->
                    DataSource.Http.get (Secrets.succeed (Api.releasesUrl author project))
                        (OptimizedDecoder.map
                            (List.map
                                (\version ->
                                    { namespace = author
                                    , package = project
                                    , version = Package.versionNumberToString version.number
                                    }
                                )
                                >> (::)
                                    { namespace = author
                                    , package = project
                                    , version = "latest"
                                    }
                            )
                            Package.decodeVersions
                        )
                )
                >> DataSource.combine
                >> DataSource.map List.concat
            )


data : RouteParams -> DataSource Data
data routeParams =
    case routeParams.version of
        "latest" ->
            DataSource.Http.get (Secrets.succeed (Api.releasesUrl routeParams.namespace routeParams.package))
                (Package.decodeVersions
                    |> OptimizedDecoder.map
                        (List.map .number
                            >> List.sortWith Package.compareVersionNumber
                            >> List.reverse
                            >> List.head
                        )
                )
                |> DataSource.andThen
                    (\maybeVersion ->
                        case maybeVersion of
                            Nothing ->
                                DataSource.fail ("No latest version found for " ++ routeParams.namespace ++ "/" ++ routeParams.package)

                            Just version ->
                                let
                                    readmeUrl : String
                                    readmeUrl =
                                        Api.readmeUrl routeParams.namespace routeParams.package (Package.versionNumberToString version)

                                    docsUrl : String
                                    docsUrl =
                                        Api.docsUrl routeParams.namespace routeParams.package (Package.versionNumberToString version)
                                in
                                DataSource.map2 Data
                                    (DataSource.Http.unoptimizedRequest
                                        (Secrets.succeed
                                            { url = readmeUrl
                                            , method = "GET"
                                            , headers = []
                                            , body = DataSource.Http.emptyBody
                                            }
                                        )
                                        (DataSource.Http.expectString Ok)
                                    )
                                    (DataSource.Http.get (Secrets.succeed docsUrl) OptimizedDecoder.value)
                    )

        _ ->
            let
                readmeUrl =
                    Api.readmeUrl routeParams.namespace routeParams.package routeParams.version

                docsUrl =
                    Api.docsUrl routeParams.namespace routeParams.package routeParams.version
            in
            DataSource.map2 Data
                (DataSource.Http.get (Secrets.succeed readmeUrl) OptimizedDecoder.string)
                (DataSource.Http.get (Secrets.succeed docsUrl) OptimizedDecoder.value)


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


type alias Data =
    { readme : String
    , docs : Value
    }


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title =
        "for "
            ++ static.routeParams.namespace
            ++ "/"
            ++ static.routeParams.package
            ++ " version "
            ++ static.routeParams.version
    , body =
        row
            [ width (fill |> maximum 800)
            , centerX
            ]
            [ case
                static.data.readme
                    |> Markdown.Parser.parse
                    |> Result.mapError deadEndsToString
                    |> Result.andThen (\ast -> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer ast)
              of
                Ok rendered ->
                    List.map html rendered
                        |> column [ width fill ]

                Err errors ->
                    text errors
            ]
    }


deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.Parser.deadEndToString
        |> String.join "\n"
