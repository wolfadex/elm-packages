module Page.Packages.Namespace_ exposing (Data, Model, Msg, page)

import Api
import DataSource exposing (DataSource)
import DataSource.Http
import Element exposing (..)
import Head
import Head.Seo as Seo
import OptimizedDecoder
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
    { namespace : String }


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
    DataSource.Http.get
        (Secrets.succeed Api.searchUrl)
        (OptimizedDecoder.list Package.decodeSummary
            |> OptimizedDecoder.map
                (List.filterMap
                    (\package ->
                        case String.split "/" package.name of
                            [ namespace, _ ] ->
                                Just { namespace = namespace }

                            _ ->
                                Nothing
                    )
                )
        )


data : RouteParams -> DataSource Data
data routeParams =
    DataSource.Http.get
        (Secrets.succeed Api.searchUrl)
        (OptimizedDecoder.list Package.decodeSummary
            |> OptimizedDecoder.map
                (List.filter
                    (\package ->
                        case String.split "/" package.name of
                            [ namespace, _ ] ->
                                routeParams.namespace == namespace

                            _ ->
                                False
                    )
                )
        )


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
    List Package.Summary


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = "for " ++ static.routeParams.namespace
    , body =
        column
            [ centerX ]
            (List.map Package.viewSummary static.data)
    }
