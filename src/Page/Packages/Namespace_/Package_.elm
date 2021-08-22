module Page.Packages.Namespace_.Package_ exposing (Data, Model, Msg, page)

import Api
import DataSource exposing (DataSource)
import DataSource.Http
import Element exposing (..)
import Element.Border as Border
import Head
import Head.Seo as Seo
import OptimizedDecoder
import Package
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Secrets as Secrets
import Pages.Url
import Shared
import Time exposing (Month(..), Posix, Zone)
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    { namespace : String, package : String }


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
                                Just { namespace = namespace, package = pkg }

                            _ ->
                                Nothing
                    )
                )
        )


data : RouteParams -> DataSource Data
data routeParams =
    DataSource.Http.get
        (Secrets.succeed (Api.releasesUrl routeParams.namespace routeParams.package))
        Package.decodeVersions
        |> DataSource.map
            (List.sortWith
                (\left right -> Package.compareVersionNumber left.number right.number)
                >> List.reverse
            )


head : StaticPayload Data RouteParams -> List Head.Tag
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
    List Package.Version


view : Maybe PageUrl -> Shared.Model -> StaticPayload Data RouteParams -> View Msg
view maybeUrl sharedModel static =
    { title = "for " ++ static.routeParams.namespace ++ "/" ++ static.routeParams.package
    , body =
        static.data
            |> List.map (viewVersion sharedModel.timeZone)
            |> column
                [ centerX
                , width (fill |> maximum 400)
                ]
    }


viewVersion : Zone -> Package.Version -> Element Msg
viewVersion zone { number, date } =
    row
        [ width fill
        , spacingXY 8 0
        ]
        [ text (Package.versionNumberToString number)
        , el
            [ width fill
            , Border.solid
            , Border.color (rgb 0.7 0.7 0.7)
            , Border.widthEach
                { top = 1
                , bottom = 0
                , left = 0
                , right = 0
                }
            ]
            none
        , text (formatDate zone date)
        ]


formatDate : Zone -> Posix -> String
formatDate zone date =
    let
        year =
            Time.toYear zone date
                |> String.fromInt

        month =
            Time.toMonth zone date
                |> monthToInt
                |> String.fromInt

        day =
            Time.toDay zone date
                |> String.fromInt
    in
    String.join "-" [ year, month, day ]


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12
