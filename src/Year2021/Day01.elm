module Year2021.Day01 exposing (..)

import Performance exposing (Performance)
import Result.Extra as Result
import Parser exposing ((|.), (|=), Parser)
import Util.Parser


solution =
    { solve = solve
    , title = "Sonar Sweep"
    , subtitle = "Scan the sea floor and filter the data."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        numbers =
            input
                |> Parser.run (Util.Parser.parseRowsUsing Parser.int)
                |> Result.mapError Util.Parser.firstErrorMsg

        r1 =
            numbers
                |> Result.map
                    (countIncreases
                        >> String.fromInt
                    )

        r2 =
            numbers
                |> Result.map
                    (toSlidingWindow3
                        >> countIncreases
                        >> String.fromInt
                    )
    in
    ( r1
    , r2
    )


toSlidingWindow3 : List Int -> List Int
toSlidingWindow3 ns =
    case ns of
        a :: b :: c :: rest ->
            a + b + c :: nextWindow b c rest

        _ ->
            []


nextWindow : Int -> Int -> List Int -> List Int
nextWindow b c rest =
    case rest of
        [] ->
            []

        x :: xs ->
            b + c + x :: nextWindow c x xs


countIncreases : List Int -> Int
countIncreases numbers =
    case numbers of
        [] ->
            0

        x :: xs ->
            countIncreasesHelp x xs


countIncreasesHelp : Int -> List Int -> Int
countIncreasesHelp prev numbers =
    case numbers of
        [] ->
            0

        x :: xs ->
            if x > prev then
                1 + countIncreasesHelp x xs

            else
                countIncreasesHelp x xs
