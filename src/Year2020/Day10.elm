module Year2020.Day10 exposing (..)

import List.Extra as List
import Performance exposing (Performance)
import Result.Extra as Result


solution =
    { solve = solve
    , title = "Adapter Array"
    , subtitle = "Get the correct joltage for your adapter."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        incPerStep =
            String.lines input
                |> List.map (String.toInt >> Result.fromMaybe "Input must be numbers")
                |> Result.combine
                |> Result.map
                    (List.sort
                        >> List.scanl
                            (\n ( prev, diff ) ->
                                ( n
                                , n - prev
                                )
                            )
                            ( 0, 0 )
                        >> List.map Tuple.second
                        >> (++) [ 3 ]
                    )

        r1 =
            incPerStep
                |> Result.map
                    (countOnesAndThrees
                        >> (\( ones, threes ) -> ones * threes)
                    )

        r2 =
            incPerStep
                |> Result.map
                    (groupByValue >> calculateCombinations)
    in
    ( Result.map String.fromInt r1
    , Result.map String.fromInt r2
    )


countOnesAndThrees : List Int -> ( Int, Int )
countOnesAndThrees list =
    List.foldl
        (\n ( ones, threes ) ->
            if n == 1 then
                ( ones + 1, threes )

            else if n == 3 then
                ( ones, threes + 1 )

            else
                ( ones, threes )
        )
        ( 0, 0 )
        list


groupByValue : List Int -> List ( Int, Int )
groupByValue list =
    case list of
        x :: xs ->
            groupByValueHelper 1 x xs

        [] ->
            []


groupByValueHelper : Int -> Int -> List Int -> List ( Int, Int )
groupByValueHelper count prev list =
    case list of
        x :: xs ->
            if prev == x then
                groupByValueHelper (count + 1) x xs

            else
                ( count, prev ) :: groupByValueHelper 1 x xs

        [] ->
            [ ( count, prev ) ]



{-
   1
       x
   1 1
       x x
       x 1

   1 1 1
       x x x
       x x 1
       x 1 x
       x 1 1

   1 1 1 1
       (x x x x)
       x x x 1
       x x 1 x
       x x 1 1
       x 1 x x
       x 1 x 1
       x 1 1 x
       x 1 1 1

   1 1 1 1 1
       (x x x x x)
       (x x x x 1)
       x x x 1 x
       x x x 1 1

       x x 1 x x
       x x 1 x 1
       x x 1 1 x
       x x 1 1 1

       x 1 x x x
       x 1 x x 1
       x 1 x 1 x
       x 1 x 1 1
       x 1 1 x x
       x 1 1 x 1
       x 1 1 1 x
       x 1 1 1 1


   1 1 1 1 1 1
       (x x x x x x)
       (x x x x x 1)
       (x x x x 1 x)
       (x x x x 1 1)
       x x x 1 x x
       x x x 1 x 1
       x x x 1 1 x
       x x x 1 1 1
       x x 1 x x x
       x x 1 x x 1
       x x 1 x 1 x
       x x 1 x 1 1
       x x 1 1 x x
       x x 1 1 x 1
       x x 1 1 1 x
       x x 1 1 1 1
       (x 1 x x x x)
       x 1 x x x 1
       x 1 x x 1 x
       x 1 x x 1 1
       x 1 x 1 x x
       x 1 x 1 x 1
       x 1 x 1 1 x
       x 1 x 1 1 1
       x 1 1 x x x
       x 1 1 x x 1
       x 1 1 x 1 x
       x 1 1 x 1 1
       x 1 1 1 x x
       x 1 1 1 x 1
       x 1 1 1 1 x
       x 1 1 1 1 1

   prob n =
       2 ^ n - ?

-}


calculateCombinations : List ( Int, Int ) -> Int
calculateCombinations list =
    case list of
        ( count, 1 ) :: rest ->
            case count of
                1 ->
                    calculateCombinations rest

                2 ->
                    2 * calculateCombinations rest

                3 ->
                    4 * calculateCombinations rest

                4 ->
                    7 * calculateCombinations rest

                _ ->
                    calculateCombinations rest

        ( count, n ) :: rest ->
            calculateCombinations rest

        [] ->
            1
