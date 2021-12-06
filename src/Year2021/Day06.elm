module Year2021.Day06 exposing (..)

import Dict exposing (Dict)
import Performance exposing (Performance)
import Result.Extra as Result


solution =
    { solve = solve
    , title = "Lanternfish"
    , subtitle = "Calculate the population of lanternfishes."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        parsedNumbers =
            String.split "," input
                |> List.map
                    (String.toInt
                        >> Result.fromMaybe "Only numbers separated with comma is allowed."
                    )
                |> Result.combine

        r1 =
            parsedNumbers
                |> Result.map
                    (\numbers ->
                        numbers
                            |> List.map (population 80)
                            |> List.sum
                            |> String.fromInt
                    )

        r2 =
            parsedNumbers
                |> Result.map
                    (\numbers ->
                        numbers
                            |> List.map (population 256)
                            |> List.sum
                            |> String.fromInt
                    )
    in
    ( r1
    , r2
    )


{-| Recalculate starting day as if we just spawned, ie. 9
days to next spawn.
-}
population : Int -> Int -> Int
population days initial =
    populationAfter (days + 8 - initial)


{-| Calculate population after N days.
First spawn after 9 days, then spawn every 7 days.

Basically, this is just a memoized version of:

    popAfter : Int -> Int
    popAfter days =
        if days < 9 then
            1

        else
            popAfter (days - 7) + popAfter (days - 9)

Does it remind you of recursive Fibonacci maybe?

-}
populationAfter : Int -> Int
populationAfter days =
    populationAfterMem Dict.empty days
        |> Tuple.first


populationAfterMem : Dict Int Int -> Int -> ( Int, Dict Int Int )
populationAfterMem mem d =
    case Dict.get d mem of
        Just v ->
            ( v, mem )

        Nothing ->
            if d < 9 then
                ( 1, Dict.insert d 1 mem )

            else
                let
                    ( v1, mem1 ) =
                        populationAfterMem mem (d - 7)

                    ( v2, mem2 ) =
                        populationAfterMem mem1 (d - 9)
                in
                ( v1 + v2
                , Dict.insert d (v1 + v2) mem2
                )
