module Year2023.Day11 exposing (..)

import List.Extra as List
import Performance
import Set exposing (Set)
import Util.Grid as Grid
import Util.List as List
import Util.Tuple as Tuple


solution =
    { solve = solve
    , title = "Cosmic Expansion"
    , subtitle = "Calculate shortest path between galaxies in an expanding universe."
    , tests = []
    , performance = Performance.Acceptable
    }


parseInput : String -> Set ( Int, Int )
parseInput =
    String.lines
        >> List.indexedMap
            (\row ->
                String.toList
                    >> List.indexedMap
                        (\col char ->
                            if char == '.' then
                                Nothing

                            else
                                Just ( row, col )
                        )
                    >> List.filterMap identity
            )
        >> List.concat
        >> Set.fromList


findGaps : Set Int -> List Int
findGaps =
    Set.toList
        >> List.mapWindow2 (\prev x -> List.range (prev + 1) (x - 1))
        >> List.concat


countBefore : Int -> List Int -> Int
countBefore n list =
    case list of
        [] ->
            0

        x :: xs ->
            if x < n then
                1 + countBefore n xs

            else
                0


solve : String -> ( Result String String, Result String String )
solve input =
    let
        galaxies : Set ( Int, Int )
        galaxies =
            parseInput input

        rowsMissing : List Int
        rowsMissing =
            Set.map Tuple.first galaxies
                |> findGaps

        colsMissing : List Int
        colsMissing =
            Set.map Tuple.second galaxies
                |> findGaps

        expandBy : Int -> Set ( Int, Int ) -> Set ( Int, Int )
        expandBy n =
            Set.map
                (\( r, c ) ->
                    ( r + countBefore r rowsMissing * (n - 1)
                    , c + countBefore c colsMissing * (n - 1)
                    )
                )

        solveFor : Int -> String
        solveFor n =
            expandBy n galaxies
                |> Set.toList
                |> List.uniquePairs
                |> List.map (\( p1, p2 ) -> Tuple.sub p1 p2)
                |> List.map Grid.length
                |> List.sum
                |> String.fromInt
    in
    ( solveFor 2
        |> Ok
    , solveFor 1000000
        |> Ok
    )
