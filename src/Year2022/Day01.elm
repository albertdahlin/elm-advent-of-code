module Year2022.Day01 exposing (..)

import Performance exposing (Performance)
import Result.Extra as Result


solution =
    { solve = solve
    , title = "Calorie Counting"
    , subtitle = "Count how many calories the elves are carrying"
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        inventories : Result String (List (List Int))
        inventories =
            String.split "\n\n" input
                |> List.map
                    (String.split "\n"
                        >> List.map (String.toInt >> Result.fromMaybe "Not a number")
                        >> Result.combine
                    )
                |> Result.combine

        r1 =
            inventories
                |> Result.map
                    (List.map List.sum
                        >> List.maximum
                        >> Maybe.withDefault 0
                        >> String.fromInt
                    )

        r2 =
            inventories
                |> Result.map
                    (List.map List.sum
                        >> List.sort
                        >> List.reverse
                        >> List.take 3
                        >> List.sum
                        >> String.fromInt
                    )
    in
    ( r1
    , r2
    )
