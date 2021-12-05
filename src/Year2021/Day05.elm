module Year2021.Day05 exposing (..)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Util.Parser
import Util.Vec2 as Vec2


solution =
    { solve = solve
    , title = "Hydrothermal Venture"
    , subtitle = "Navigate across a field of hydrothermal vents by plotting lines."
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Line =
    ( Point, Point )


type alias Point =
    ( Int, Int )


solve : String -> ( Result String String, Result String String )
solve input =
    let
        parsedLines =
            Util.Parser.run
                (Util.Parser.parseRowsUsing lineParser
                    |. Parser.end
                )
                input

        r1 =
            parsedLines
                |> Result.map
                    (\lines ->
                        lines
                            |> List.filter isOrthogonal
                            |> plotLines
                            |> Dict.filter (\_ v -> v > 1)
                            |> Dict.size
                            |> String.fromInt
                    )

        r2 =
            parsedLines
                |> Result.map
                    (\lines ->
                        lines
                            |> List.filter (\line -> isOrthogonal line || isDiagonal line)
                            |> plotLines
                            |> Dict.filter (\_ v -> v > 1)
                            |> Dict.size
                            |> String.fromInt
                    )
    in
    ( r1
    , r2
    )



-- PLOT


{-| Plot lines in a sparse grid.
-}
plotLines : List Line -> Dict Point Int
plotLines lines =
    List.foldl
        (\line dict ->
            toPoints line
                |> List.foldl
                    incPointByOne
                    dict
        )
        Dict.empty
        lines


{-| Increase value for a point by 1.
-}
incPointByOne : Point -> Dict Point Int -> Dict Point Int
incPointByOne point dict =
    Dict.update
        point
        (\mbCount ->
            case mbCount of
                Just count ->
                    Just (count + 1)

                Nothing ->
                    Just 1
        )
        dict



-- LINE


{-| Rasterize a line to points.
-}
toPoints : Line -> List Point
toPoints ( p1, p2 ) =
    let
        ( x1, y1 ) =
            p1

        ( dX, dY ) =
            Vec2.sub p1 p2

        steps =
            max (abs dX) (abs dY)

        scaleX =
            toFloat dX / toFloat steps

        scaleY =
            toFloat dY / toFloat steps
    in
    List.range 0 steps
        |> List.map
            (\step ->
                ( x1 + round (scaleX * toFloat step)
                , y1 + round (scaleY * toFloat step)
                )
            )


isOrthogonal : Line -> Bool
isOrthogonal ( ( x1, y1 ), ( x2, y2 ) ) =
    y1 == y2 || x1 == x2


isDiagonal : Line -> Bool
isDiagonal ( p1, p2 ) =
    let
        ( dX, dY ) =
            Vec2.sub p1 p2
    in
    abs dX == abs dY



-- PARSER


lineParser : Parser Line
lineParser =
    Parser.succeed Tuple.pair
        |= pointParser
        |. Parser.spaces
        |. Parser.symbol "->"
        |. Parser.spaces
        |= pointParser


pointParser : Parser Point
pointParser =
    Parser.succeed Tuple.pair
        |= Parser.int
        |. Parser.symbol ","
        |= Parser.int
