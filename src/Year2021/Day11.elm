module Year2021.Day11 exposing (..)

import Dict exposing (Dict)
import Performance exposing (Performance)
import Result.Extra as Result
import Util.Char
import Util.Grid2 as Grid
import Util.Repeat as Repeat


solution =
    { solve = solve
    , title = "Dumbo Octopus"
    , subtitle = "Navigate thorugh the cave by couting flasing octopuses."
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Grid =
    Dict ( Int, Int ) Int


solve : String -> ( Result String String, Result String String )
solve input =
    let
        parsed =
            parse input

        r1 =
            parsed
                |> Result.map
                    (\grid ->
                        ( 0, grid )
                            |> Repeat.nTimes 100
                                (\( count, g1 ) ->
                                    let
                                        g2 =
                                            tick g1
                                    in
                                    ( count + countFlash g2
                                    , resetFlash g2
                                    )
                                )
                            |> Tuple.first
                            |> String.fromInt
                    )

        r2 =
            parsed
                |> Result.map
                    (\grid ->
                        ( 1, grid )
                            |> Repeat.whileJust
                                (\( count, g1 ) ->
                                    let
                                        g2 =
                                            tick g1
                                    in
                                    if Grid.all (\_ -> isFlashing) g2 then
                                        Nothing

                                    else
                                        ( count + 1
                                        , resetFlash g2
                                        )
                                            |> Just
                                )
                            |> Tuple.first
                            |> String.fromInt
                    )
    in
    ( r1
    , r2
    )


isFlashing : Int -> Bool
isFlashing v =
    v >= 100


countFlash : Grid -> Int
countFlash grid =
    Dict.foldl
        (\_ val c ->
            if isFlashing val then
                c + 1

            else
                c
        )
        0
        grid


resetFlash : Grid -> Grid
resetFlash =
    Dict.map
        (\_ v ->
            if isFlashing v then
                0

            else
                v
        )


tick : Grid -> Grid
tick grid =
    Dict.foldl
        (\pos _ grid2 ->
            case Dict.get pos grid2 of
                Just val ->
                    if val > 9 then
                        checkFlash pos val grid2

                    else
                        grid2

                Nothing ->
                    grid2
        )
        (incAll grid)
        grid


checkFlash : ( Int, Int ) -> Int -> Grid -> Grid
checkFlash pos val grid =
    if isFlashing val then
        grid

    else if val > 9 then
        Dict.insert pos 100 grid
            |> Grid.foldAround pos incAt
            |> Grid.foldAround pos checkFlash

    else
        grid


incAll : Grid -> Grid
incAll =
    Dict.map (\_ v -> v + 1)


incAt : ( Int, Int ) -> Int -> Grid -> Grid
incAt point _ grid =
    Grid.updateAt point ((+) 1) grid



-- PARSE


parse : String -> Result String Grid
parse input =
    Grid.forLinesAndChars input
        (\pos char dict ->
            case Util.Char.toInt char of
                Just n ->
                    Dict.insert pos n dict

                Nothing ->
                    dict
        )
        Dict.empty
        |> Ok
