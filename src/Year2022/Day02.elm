module Year2022.Day02 exposing (..)

import Performance exposing (Performance)
import Result.Extra as Result


solution =
    { solve = solve
    , title = "Rock Paper Scissors"
    , subtitle = "Calculate your score from an encrypted guide."
    , tests = []
    , performance = Performance.Acceptable
    }


type Shape
    = Rock
    | Paper
    | Scissor


type Outcome
    = Win
    | Draw
    | Loss


outcomeOf : Shape -> Shape -> Outcome
outcomeOf me oponent =
    case me of
        Rock ->
            case oponent of
                Rock ->
                    Draw

                Paper ->
                    Loss

                Scissor ->
                    Win

        Paper ->
            case oponent of
                Rock ->
                    Win

                Paper ->
                    Draw

                Scissor ->
                    Loss

        Scissor ->
            case oponent of
                Rock ->
                    Loss

                Paper ->
                    Win

                Scissor ->
                    Draw


outcomeScore : Outcome -> Int
outcomeScore outcome =
    case outcome of
        Win ->
            6

        Draw ->
            3

        Loss ->
            0


handScore : Shape -> Int
handScore hand =
    case hand of
        Rock ->
            1

        Paper ->
            2

        Scissor ->
            3


myHand : Shape -> Outcome -> Shape
myHand oponent outcome =
    case oponent of
        Rock ->
            case outcome of
                Win ->
                    Paper

                Loss ->
                    Scissor

                Draw ->
                    Rock

        Paper ->
            case outcome of
                Win ->
                    Scissor

                Loss ->
                    Rock

                Draw ->
                    Paper

        Scissor ->
            case outcome of
                Win ->
                    Rock

                Loss ->
                    Paper

                Draw ->
                    Scissor


parseWith :
    (String -> Result String a)
    -> (String -> Result String b)
    -> String
    -> Result String (List ( a, b ))
parseWith parseFirst parseSecond input =
    String.lines input
        |> List.map
            (\line ->
                case String.split " " line of
                    [ a, b ] ->
                        Result.map2
                            Tuple.pair
                            (parseFirst a)
                            (parseSecond b)

                    _ ->
                        Err "Could not parse line"
            )
        |> Result.combine


parseOponent : String -> Result String Shape
parseOponent c =
    case c of
        "A" ->
            Ok Rock

        "B" ->
            Ok Paper

        "C" ->
            Ok Scissor

        _ ->
            Err c


parseMe : String -> Result String Shape
parseMe c =
    case c of
        "X" ->
            Ok Rock

        "Y" ->
            Ok Paper

        "Z" ->
            Ok Scissor

        _ ->
            Err c


parseOutcome : String -> Result String Outcome
parseOutcome c =
    case c of
        "X" ->
            Ok Loss

        "Y" ->
            Ok Draw

        "Z" ->
            Ok Win

        _ ->
            Err c


solve : String -> ( Result String String, Result String String )
solve input =
    let
        r1 =
            parseWith parseOponent parseMe input
                |> Result.map
                    (\hands ->
                        List.foldl
                            (\( oponent, me ) total ->
                                total
                                    + outcomeScore (outcomeOf me oponent)
                                    + handScore me
                            )
                            0
                            hands
                            |> String.fromInt
                    )

        r2 =
            parseWith parseOponent parseOutcome input
                |> Result.map
                    (\hands ->
                        List.foldl
                            (\( oponent, outcome ) total ->
                                total
                                    + outcomeScore outcome
                                    + handScore (myHand oponent outcome)
                            )
                            0
                            hands
                            |> String.fromInt
                    )
    in
    ( r1
    , r2
    )
