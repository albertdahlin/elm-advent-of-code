module Year2020.Day01 exposing (..)

import Performance exposing (Performance)
import List.Extra as List
import Result.Extra as Result


solution =
    { solve = solve
    , title = "Report Repair"
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        numbers =
            String.lines input
                |> List.map (String.toInt >> Result.fromMaybe "Only numbers allowed")
                |> Result.combine

        r1 =
            numbers
                |> Result.map
                    (findTwo 2020
                        >> Result.fromMaybe "No number sums to 2020"
                    )
                |> Result.andThen identity
                |> Result.map
                    (\( x, y ) ->
                        [ String.fromInt x
                        , "*"
                        , String.fromInt y
                        , "="
                        , String.fromInt (x * y)
                        ]
                            |> String.join " "
                    )

        r2 =
            numbers
                |> Result.map
                    (findThree 2020
                        >> Result.fromMaybe "No number sums to 2020"
                    )
                |> Result.andThen identity
                |> Result.map
                    (\( x, y, z ) ->
                        [ String.fromInt x
                        , "*"
                        , String.fromInt y
                        , "*"
                        , String.fromInt z
                        , "="
                        , String.fromInt (x * y * z)
                        ]
                            |> String.join " "
                    )
    in
    ( r1
    , r2
    )


findTwo : Int -> List Int -> Maybe ( Int, Int )
findTwo sum list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            case List.find (\y -> x + y == sum) xs of
                Just y ->
                    Just ( x, y )

                Nothing ->
                    findTwo sum xs


findThree : Int -> List Int -> Maybe ( Int, Int, Int )
findThree sum list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            case findTwo (sum - x) xs of
                Just ( y, z ) ->
                    Just ( x, y, z )

                Nothing ->
                    findThree sum xs
