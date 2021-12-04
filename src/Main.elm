module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Performance
import Solution exposing (Solution)
import Url exposing (Url)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }


type Msg
    = UrlChanged Url
    | UrlRequested Browser.UrlRequest
    | InputChanged String


type alias Model =
    { year : Int
    , day : Int
    , input : Dict ( Int, Int ) String
    , solution1 : Dict ( Int, Int ) (Result String String)
    , solution2 : Dict ( Int, Int ) (Result String String)
    , key : Nav.Key
    }


type alias Flags =
    ()


initModel : Url -> Nav.Key -> Model
initModel url key =
    let
        ( year, day, input ) =
            parseUrl url
    in
    { year = year
    , day = day
    , input = Dict.singleton ( year, day ) input
    , solution1 = Dict.empty
    , solution2 = Dict.empty
    , key = key
    }
        |> solveFor input


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( initModel url key
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Cmd.none
                    )

        UrlChanged url ->
            let
                ( year, day, input ) =
                    parseUrl url
            in
            ( { model
                | year = year
                , day = day
                , input = Dict.insert ( year, day ) input model.input
              }
                |> solveFor input
            , Cmd.none
            )

        InputChanged str ->
            ( { model
                | input =
                    Dict.insert
                        ( model.year, model.day )
                        str
                        model.input
                , solution1 =
                    Dict.insert
                        ( model.year, model.day )
                        (Err "--calculating--")
                        model.solution1
                , solution2 =
                    Dict.insert
                        ( model.year, model.day )
                        (Err "--calculating--")
                        model.solution2
              }
            , toLink model.year (Just model.day) str
                |> Nav.replaceUrl model.key
            )


solveFor : String -> Model -> Model
solveFor input model =
    if String.isEmpty input then
        { model
            | solution1 = Dict.insert ( model.year, model.day ) (Err "--empty--") model.solution1
            , solution2 = Dict.insert ( model.year, model.day ) (Err "--empty--") model.solution2
        }

    else
        case
            Solution.for model.year model.day
        of
            Just solution ->
                let
                    ( r1, r2 ) =
                        solution.solve input
                in
                { model
                    | solution1 = Dict.insert ( model.year, model.day ) r1 model.solution1
                    , solution2 = Dict.insert ( model.year, model.day ) r2 model.solution2
                }

            Nothing ->
                model


parseUrl : Url -> ( Int, Int, String )
parseUrl url =
    case String.split "/" (Maybe.withDefault "" url.fragment) of
        [ year ] ->
            ( String.toInt year |> Maybe.withDefault 0
            , 1
            , ""
            )

        [ year, day ] ->
            ( String.toInt year |> Maybe.withDefault 0
            , String.toInt day |> Maybe.withDefault 1
            , ""
            )

        [ year, day, input ] ->
            ( String.toInt year |> Maybe.withDefault 0
            , String.toInt day |> Maybe.withDefault 1
            , Url.percentDecode input |> Maybe.withDefault ""
            )

        _ ->
            ( 0, 0, "" )


view : Model -> Browser.Document Msg
view model =
    let
        title =
            Solution.for model.year model.day
                |> Maybe.map .title
                |> Maybe.withDefault ""
    in
    { title =
        case Solution.for model.year model.day of
            Just solution ->
                "Advent of Code Day " ++ String.fromInt model.day ++ " - " ++ solution.title

            Nothing ->
                "Advent of Code"
    , body =
        [ Html.div
            []
            [ Html.node "style" [] [ Html.text css ]
            , Html.div
                [ HA.class "column pad-md space-lg"
                ]
                [ view_Header model
                , view_Body model
                , view_Footer model
                ]
            ]
        ]
    }


view_Header : Model -> Html Msg
view_Header model =
    let
        availableSolutions d =
            case Solution.for model.year d of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    [ List.range 2015 2021
        |> List.map
            (\y ->
                Html.a
                    [ classIf (y == model.year) "active"
                    , HA.href (toLink y (Just 1) "")
                    ]
                    [ Html.text (String.fromInt y)
                    ]
            )
        |> Html.div
            [ HA.class "row space-md center-x"
            ]
    , List.range 1 25
        |> List.map
            (\d ->
                case Solution.for model.year d of
                    Just solution ->
                        Html.a
                            [ classIf (d == model.day) "active"
                            , case solution.performance of
                                Performance.Acceptable ->
                                    HA.style "" ""

                                Performance.Bad ->
                                    HA.class "yellow"

                                Performance.Terrible ->
                                    HA.class "yellow"
                            , HA.href (toLink model.year (Just d) "")
                            ]
                            [ Html.text (String.fromInt d)
                            ]

                    Nothing ->
                        Html.div
                            []
                            [ Html.text (String.fromInt d)
                            ]
            )
        |> Html.div
            [ HA.class "row space-md center-x"
            ]
    ]
        |> Html.header
            [ HA.class "column space-sm pad-sm"
            ]


view_Body : Model -> Html Msg
view_Body model =
    let
        maybeSolution =
            Solution.for model.year model.day
    in
    case maybeSolution of
        Nothing ->
            Html.text ""

        Just solution ->
            Html.main_
                [ HA.class "column space-lg"
                ]
                [ Html.div
                    []
                    [ Html.text ("--- Day " ++ String.fromInt model.day ++ ": " ++ solution.title ++ " ---")
                    ]
                , view_Description model solution
                , view_Solution model
                ]


view_Description : Model -> Solution -> Html Msg
view_Description model solution =
    Html.div
        [ HA.class "column space-md"
        ]
        [ Html.div [] [ Html.text solution.subtitle ]
        , Html.div
            [ HA.class "column"
            ]
            [ Html.div
                []
                [ Html.text "- Read the "
                , Html.a
                    [ HA.href (linkToAdventOfCode model.year model.day)
                    , HA.target "_blank"
                    ]
                    [ Html.text "problem description"
                    ]
                , Html.text " on adventofcode.com"
                ]
            , Html.div
                [
                ]
                [ Html.text "- Check my "
                , Html.a
                    [ HA.href (linkToGithub model.year model.day)
                    , HA.target "_blank"
                    ]
                    [ Html.text "solution"
                    ]
                , Html.text " on github.com"
                ]
            ]
        ]


view_Solution : Model -> Html Msg
view_Solution model =
    let
        r1 =
            Dict.get ( model.year, model.day ) model.solution1
                |> Maybe.withDefault (Err "--none--")

        r2 =
            Dict.get ( model.year, model.day ) model.solution2
                |> Maybe.withDefault (Err "--none--")

        input =
            Dict.get ( model.year, model.day ) model.input
                |> Maybe.withDefault ""
    in
    Html.div
        [ HA.class "column space-md"
        ]
        [ Html.div [] [ Html.text "--- Solution ---" ]
        , view_ResultRow r1 r2
        , view_Input input
        ]


view_Input : String -> Html Msg
view_Input input =
    Html.label
        [ HA.class "column space-sm"
        ]
        [ Html.div
            []
            [ Html.text "Input"
            ]
        , Html.textarea
            [ HA.value input
            , Events.onInput InputChanged
            ]
            [ Html.text input
            ]
        ]


view_ResultRow : Result String String -> Result String String -> Html Msg
view_ResultRow r1 r2 =
    Html.div
        [ HA.class "column space-sm"
        ]
        [ view_Result "Part 1" r1
        , view_Result "Part 2" r2
        ]


view_Result : String -> Result String String -> Html Msg
view_Result label result =
    Html.div
        [ HA.class "row space-sm center-y"
        ]
        [ Html.div
            []
            [ Html.text (label ++ ":")
            ]
        , case result of
            Ok value ->
                Html.div
                    [ HA.class "look-like-ok"
                    ]
                    [ Html.text value
                    ]

            Err message ->
                Html.div
                    [ HA.class "look-like-err"
                    ]
                    [ Html.text message
                    ]
        ]


view_Footer : Model -> Html msg
view_Footer model =
    Html.footer
        [ HA.class "row footer right"
        ]
        [ Html.text "albertdahlin.com"
        ]


toLink : Int -> Maybe Int -> String -> String
toLink y mbDay input =
    case mbDay of
        Just d ->
            "#"
                ++ String.fromInt y
                ++ "/"
                ++ String.fromInt d
                ++ (if String.isEmpty input then
                        ""

                    else
                        "/" ++ Url.percentEncode input
                   )

        Nothing ->
            "#"
                ++ String.fromInt y


linkToAdventOfCode : Int -> Int -> String
linkToAdventOfCode year day =
    "https://adventofcode.com/" ++ String.fromInt year ++ "/day/" ++ String.fromInt day


linkToGithub : Int -> Int -> String
linkToGithub year day =
    "https://github.com/albertdahlin/elm-advent-of-code/blob/master/src/Year"
        ++ String.fromInt year
        ++ "/Day"
        ++ (String.fromInt day |> String.padLeft 2 '0')
        ++ ".elm"


classIf : Bool -> String -> Html.Attribute msg
classIf pred name =
    if pred then
        HA.class name

    else
        HA.style "" ""


css : String
css =
    """
body {
    font-family: monospace;
    font-size: 18px;
    background: #0f0f23;
    color: #cccccc;
    line-height: 1.15;
}

.footer {
    position: fixed;
    bottom: 0.5em;
    align-self: flex-end;
}

.row {
    display: flex;
    flex-direction: row;
}
.row.space-sm > * + * {
    margin-left: 0.5em;
}
.row.space-md > * + * {
    margin-left: 1em;
}
.row.center-x {
    justify-content: center;
}
.row.right {
    justify-content: flex-end;
}
.row.center-y {
    align-items: center;
}


.column {
    display: flex;
    flex-direction: column;
}
.column.space-sm > * + * {
    margin-top: 0.5em;
}
.column.space-md > * + * {
    margin-top: 1em;
}
.column.space-lg > * + * {
    margin-top: 2em;
}
.column.center-y {
    justify-content: center;
}
.column.center-x {
    align-items: center;
}



.pad-sm {
    padding: 0.5em;
}
.pad-md {
    padding: 1em;
}
.pad-lg {
    padding: 2em;
}


a {
    color: #009900;
    cursor: pointer;
    text-decoration: none;
}
a:not(.disabled):hover {
    color: #99ff99;
}
a:not(.disabled).active {
    color: #99ff99;
}
a.yellow {
    color: #ffff66;
}

.glowing-green {
    color: #00cc00;
    text-shadow: 0 0 2px #00cc00, 0 0 5px #00cc00;
}
.glowing-yellow {
    color: #ffff66;
    text-shadow: 0 0 5px #ffff66;
}


.look-like-ok
, .look-like-err {
    background: #111;
    outline: solid 1px #333;
    padding: 0.2em;
}
.look-like-ok {
    color: #009900;
}
.look-like-err {
    color: #ffaaaa;
}

hr {
    margin: 0;
    border: solid 1px #777;
}
textarea {
    background: #0c0c20;
    border-color: #555;
    color: #ccc;
    font-family: monospace;
    height: 20em;
    width: 100%;
}
textarea:focus {
    border-color: #777;
    outline: none;
}
"""
