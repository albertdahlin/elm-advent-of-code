module Year2020.Day15 exposing (..)

import Dict exposing (Dict)
import Performance exposing (Performance)
import Result.Extra as Result


solution =
    { solve = solve
    , title = "Rambunctious Recitation (slow)"
    , subtitle = "Play number memory with the Elves."
    , tests = []
    , performance = Performance.Bad
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        result : Result String (List Int)
        result =
            String.split "," input
                |> List.map (String.toInt >> Result.fromMaybe "Not a number")
                |> Result.combine

        startingCondition : Result String ( Int, Int, Dict Int Int )
        startingCondition =
            result
                |> Result.map
                    (List.foldl
                        (\n ( prev, idx, dict ) ->
                            ( n
                            , idx + 1
                            , Dict.insert prev idx dict
                            )
                        )
                        ( 0, 0, Dict.empty )
                    )

        r1 =
            startingCondition
                |> Result.map
                    (\( lastN, idx, state ) ->
                        stepUntil 2020 (idx + 1) lastN state
                    )

        r2 =
            startingCondition
                |> Result.map
                    (\( lastN, idx, state ) ->
                        stepUntil 30000000 (idx + 1) lastN state
                    )
    in
    ( Result.map String.fromInt r1
    , Result.map String.fromInt r2
    )


stepUntil : Int -> Int -> Int -> Dict Int Int -> Int
stepUntil stopAt idx last state =
    if idx > stopAt then
        last

    else
        case Dict.get last state of
            Just atTurn ->
                let
                    newN =
                        idx - atTurn - 1
                in
                stepUntil stopAt (idx + 1) newN (Dict.insert last (idx - 1) state)

            Nothing ->
                stepUntil stopAt (idx + 1) 0 (Dict.insert last (idx - 1) state)
