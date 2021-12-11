module Year2019.Day01 exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Result.Extra as Result
import Util.Parser


solution =
    { solve = solve
    , title = "The Tyranny of the Rocket Equation"
    , subtitle = "Calculate fuel requirements."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        numbers =
            input
                |> String.lines
                |> List.filterMap String.toInt

        r1 =
            numbers
                |> List.map fuel
                |> List.sum
                |> String.fromInt
                |> Ok


        r2 =
            numbers
                |> List.map fuel2
                |> List.sum
                |> String.fromInt
                |> Ok
    in
    ( r1
    , r2
    )


fuel : Int -> Int
fuel m =
    m // 3 - 2


fuel2 : Int -> Int
fuel2 m =
    let
        f = max 0 (fuel m)
    in
    if f == 0 then
        0

    else
        f + fuel2 f
