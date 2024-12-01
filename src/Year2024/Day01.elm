module Year2024.Day01 exposing (..)

import Performance
import Dict exposing (Dict)
import Util.Tuple as Tuple
import Util.List as List
import List.Extra


solution =
    { solve = solve
    , title = "Cosmic Expansion"
    , subtitle = "Calculate shortest path between galaxies in an expanding universe."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        rows =
            String.lines input
                |> List.map
                    (String.words
                        >> List.map
                            (String.toInt >> Maybe.withDefault 0)
                    )
                |> List.filterMap (List.when2 Tuple.pair)

        left =
            List.map Tuple.first rows
                |> List.sort

        right =
            List.map Tuple.second rows
                |> List.sort

        freq =
            List.foldl
                (\n dict ->
                    Dict.update
                        n
                        (\m ->
                            case m of
                                Just x ->
                                    Just (x + 1)

                                Nothing ->
                                    Just 1
                        )
                        dict
                )
                Dict.empty
                right

        r1 =
            List.map2
                (\l r ->
                    abs (l - r)
                )
                left
                right
                |> List.sum
                |> String.fromInt
                |> Ok

        r2 =
            List.foldl
                (\n s ->
                    Dict.get n freq
                        |> Maybe.withDefault 0
                        |> (*) n
                        |> (+) s
                )
                0
                left
                |> String.fromInt
                |> Ok
    in
    ( r1
    , r2
    )
