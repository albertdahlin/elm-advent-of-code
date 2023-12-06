module Year2023.Day06 exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Util.Parser


solution =
    { solve = solve
    , title = "Wait For It"
    , subtitle = "Try to get as far as possible racing toy boats."
    , tests = []
    , performance = Performance.Acceptable
    }


parseInput : String -> Result String (List ( Int, Int ))
parseInput =
    Util.Parser.run
        (Parser.succeed
            (\times distances ->
                List.map2 Tuple.pair times distances
            )
            |. Parser.token "Time"
            |. Parser.symbol ":"
            |. Parser.spaces
            |= Util.Parser.listWhile
                (Util.Parser.int
                    |. Parser.spaces
                )
            |. Parser.token "Distance"
            |. Parser.symbol ":"
            |. Parser.spaces
            |= Util.Parser.listWhile
                (Util.Parser.int
                    |. Parser.spaces
                )
        )


{-| Wins are symetrical so we only need to count up to (time / 2).

dist t v = (t - v) * v

if time is even we'll get an odd number of distances.

dist 4 0 = 0
dist 4 1 = 3

dist 4 2 = 4

dist 4 3 = 3
dist 4 4 = 0

if time is odd we'll get an even number of distances.

dist 5 0 = 0
dist 5 1 = 4
dist 5 2 = 6

dist 5 3 = 6
dist 5 4 = 4
dist 5 5 = 0

-}
countWins : ( Int, Int ) -> Int
countWins ( time, dist ) =
    countWinsHelp 0 1 (toFloat time / 2 |> ceiling) ( time, dist )


countWinsHelp : Int -> Int -> Int -> ( Int, Int ) -> Int
countWinsHelp winCount speed stopAt ( time, dist ) =
    let
        mydist =
            (time - speed) * speed
    in
    if speed >= stopAt then
        -- Add one if time is even
        2 * winCount + (1 - modBy 2 time)

    else if mydist <= dist then
        countWinsHelp winCount (speed + 1) stopAt ( time, dist )

    else
        countWinsHelp (winCount + 1) (speed + 1) stopAt ( time, dist )



-- PART 1


solve1 : List ( Int, Int ) -> String
solve1 input =
    List.map countWins input
        |> List.product
        |> String.fromInt



-- PART 2


fixKerning : List ( Int, Int ) -> ( Int, Int )
fixKerning list =
    let
        numberOfDigitsIn num =
            floor (logBase 10 (toFloat num)) + 1

        concat a b =
            a * (10 ^ numberOfDigitsIn b) + b
    in
    case list of
        [] ->
            ( 0, 0 )

        pair :: rest ->
            List.foldl
                (\( time, dist ) ( totalTime, totalDist ) ->
                    ( concat totalTime time
                    , concat totalDist dist
                    )
                )
                pair
                rest


solve2 : List ( Int, Int ) -> String
solve2 input =
    fixKerning input
        |> countWins
        |> String.fromInt


solve : String -> ( Result String String, Result String String )
solve input =
    let
        parsedInput =
            parseInput input
    in
    ( Result.map solve1 parsedInput
    , Result.map solve2 parsedInput
    )
