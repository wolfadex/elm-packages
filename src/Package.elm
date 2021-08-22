module Package exposing (Summary, Version, compareVersionNumber, decodeSummary, decodeVersions, versionNumberToString, viewSummary)

import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Html.Events exposing (onFocus)
import OptimizedDecoder exposing (Decoder)
import Time exposing (Posix)


type alias Summary =
    { name : String
    , summary : String
    , license : String
    , version : VersionNumber
    }


type alias Version =
    { number : VersionNumber
    , date : Posix
    }


decodeVersions : Decoder (List Version)
decodeVersions =
    OptimizedDecoder.keyValuePairs
        (OptimizedDecoder.map ((*) 1000 >> Time.millisToPosix) OptimizedDecoder.int)
        |> OptimizedDecoder.andThen
            (\versions ->
                let
                    result : Result String (List Version)
                    result =
                        List.foldr
                            (\( numStr, date ) ->
                                Result.andThen
                                    (\prev ->
                                        Result.map
                                            (\num -> { number = num, date = date } :: prev)
                                            (parseVersionNumber numStr)
                                    )
                            )
                            (Ok [])
                            versions
                in
                case result of
                    Ok val ->
                        OptimizedDecoder.succeed val

                    Err err ->
                        OptimizedDecoder.fail err
            )


type alias VersionNumber =
    { major : Int
    , minor : Int
    , patch : Int
    }


compareVersionNumber : VersionNumber -> VersionNumber -> Order
compareVersionNumber left right =
    if left.major > right.major then
        GT

    else if left.major < right.major then
        LT

    else if left.minor > right.minor then
        GT

    else if left.minor < right.minor then
        LT

    else if left.patch > right.patch then
        GT

    else if left.patch < right.patch then
        LT

    else
        EQ


versionNumberToString : VersionNumber -> String
versionNumberToString { major, minor, patch } =
    String.join "."
        [ String.fromInt major
        , String.fromInt minor
        , String.fromInt patch
        ]


decodeVersionNumber : Decoder VersionNumber
decodeVersionNumber =
    OptimizedDecoder.andThen
        (\versionStr ->
            case parseVersionNumber versionStr of
                Err err ->
                    OptimizedDecoder.fail err

                Ok version ->
                    OptimizedDecoder.succeed version
        )
        OptimizedDecoder.string


parseVersionNumber : String -> Result String VersionNumber
parseVersionNumber str =
    case String.split "." str of
        [ majorStr, minorStr, patchStr ] ->
            Maybe.map3 VersionNumber
                (String.toInt majorStr)
                (String.toInt minorStr)
                (String.toInt patchStr)
                |> Result.fromMaybe ("Expected a semantic version but found: " ++ str)

        _ ->
            Err ("Expected a semantic version but found: " ++ str)


decodeSummary : Decoder Summary
decodeSummary =
    OptimizedDecoder.map4 Summary
        (OptimizedDecoder.field "name" OptimizedDecoder.string)
        (OptimizedDecoder.field "summary" OptimizedDecoder.string)
        (OptimizedDecoder.field "license" OptimizedDecoder.string)
        (OptimizedDecoder.field "version" decodeVersionNumber)


viewSummary : Summary -> Element msg
viewSummary package =
    let
        multipleVersions =
            if versionNumberToString package.version /= "1.0.0" then
                "..."

            else
                ""
    in
    column
        [ paddingXY 0 16
        , width fill
        , Border.solid
        , Border.color (rgb 0.7 0.7 0.7)
        , Border.widthEach
            { top = 0
            , bottom = 1
            , left = 0
            , right = 0
            }
        ]
        [ row
            [ width fill ]
            [ link
                [ width fill
                , Font.color (rgb 0 0.2 0.9)
                , Font.underline
                ]
                { label = text package.name
                , url = "/packages/" ++ package.name ++ "/latest"
                }
            , (multipleVersions ++ versionNumberToString package.version)
                |> text
                |> el [ alignRight ]
            ]
        , text package.summary
            |> el
                [ Font.size 16
                , paddingEach
                    { top = 8
                    , bottom = 0
                    , left = 0
                    , right = 0
                    }
                ]
        ]
