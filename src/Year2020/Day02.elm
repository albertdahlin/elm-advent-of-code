module Year2020.Day02 exposing (..)

import Performance exposing (Performance)
import List.Extra as List
import Regex
import Result.Extra as Result


solution =
    { solve = solve
    , title = "Password Philosophy"
    , subtitle = "Check which passwords are valid according to the Official Toboggan Corporate Policy."
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Policy =
    { min : Int
    , max : Int
    , char : String
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        rows =
            String.lines input
                |> List.map parseRow
                |> Result.combine

        isPwValid1 policy pw =
            let
                count =
                    Regex.find
                        (Regex.fromString ("[" ++ policy.char ++ "]")
                            |> Maybe.withDefault Regex.never
                        )
                        pw
                        |> List.length
            in
            count >= policy.min && count <= policy.max

        r1 =
            rows
                |> Result.map
                    (List.filter
                        (\( policy, pw ) ->
                            isPwValid1 policy pw
                        )
                        >> List.length
                        >> (\len -> String.fromInt len ++ " valid passwords")
                    )

        isPwValid2 policy pw =
            let
                first =
                    String.dropLeft (policy.min - 1) pw
                        |> String.left 1

                second =
                    String.dropLeft (policy.max - 1) pw
                        |> String.left 1
            in
            if policy.char == first then
                policy.char /= second
            else if policy.char == second then
                policy.char /= first
            else
                False

        r2 =
            rows
                |> Result.map
                    (List.filter
                        (\( policy, pw ) ->
                            isPwValid2 policy pw
                        )
                        >> List.length
                        >> (\len -> String.fromInt len ++ " valid passwords")
                    )
    in
    ( r1
    , r2
    )


parseRow : String -> Result String ( Policy, String )
parseRow row =
    case Regex.find rowRegex row of
        [ match ] ->
            case match.submatches of
                [ mbLow, mbHigh, mbChar, mbPw ] ->
                    Maybe.map4
                        (\low high char pw ->
                            ( { min = low, max = high, char = char }
                            , pw
                            )
                        )
                        (mbLow |> Maybe.andThen String.toInt)
                        (mbHigh |> Maybe.andThen String.toInt)
                        mbChar
                        mbPw
                        |> Result.fromMaybe "Wrong format"

                _ ->
                    Err ("Row error: " ++ row)

        _ ->
            Err ("Row error: " ++ row)


rowRegex =
    Regex.fromString "^(\\d+)-(\\d+)\\s([a-z]):\\s([a-z]+)$"
        |> Maybe.withDefault Regex.never
