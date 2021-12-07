module Year2021.Day07 exposing (..)

import Performance exposing (Performance)
import Result.Extra as Result


solution =
    { solve = solve
    , title = "The Treachery of Whales"
    , subtitle = "Align the crab submarine to escape being eaten by a whale."
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

        lowestR =
            parsedNumbers
                |> Result.andThen
                    (List.minimum >> Result.fromMaybe "No lowest number")

        highestR =
            parsedNumbers
                |> Result.andThen
                    (List.maximum >> Result.fromMaybe "No highest number")

        r1 =
            Result.map3
                (\numbers lowest highest ->
                    List.range lowest highest
                        |> List.map (fuelNeeded numbers >> List.sum)
                        |> List.minimum
                        |> Result.fromMaybe "No lowest fuel"
                        |> Result.map String.fromInt
                )
                parsedNumbers
                lowestR
                highestR
                |> Result.andThen identity

        r2 =
            Result.map3
                (\numbers lowest highest ->
                    List.range lowest highest
                        |> List.map (fuelNeeded numbers >> List.map sumUpTo >> List.sum)
                        |> List.minimum
                        |> Result.fromMaybe "No lowest fuel"
                        |> Result.map String.fromInt
                )
                parsedNumbers
                lowestR
                highestR
                |> Result.andThen identity
    in
    ( r1
    , r2
    )


fuelNeeded : List Int -> Int -> List Int
fuelNeeded list target =
    List.map (\v -> abs (v - target)) list


sumUpTo : Int -> Int
sumUpTo n =
    (toFloat n / 2) * toFloat (n + 1) |> round
