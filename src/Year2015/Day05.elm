module Year2015.Day05 exposing (..)

import Performance exposing (Performance)
import Regex


solution =
    { solve = solve
    , title = "Doesn't He Have Intern-Elves For This?"
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        lines =
            String.lines input

        forbiddenWords =
            Regex.fromString "ab|cd|pq|xy"
                |> Maybe.withDefault Regex.never

        anyVowel =
            Regex.fromString "[aeiou]"
                |> Maybe.withDefault Regex.never

        letterPair =
            Regex.fromString "(.)\\1"
                |> Maybe.withDefault Regex.never

        isNice1 str =
            (List.length (Regex.find anyVowel str) >= 3)
                && (Regex.contains letterPair str)
                && (not <| Regex.contains forbiddenWords str)

        r1 =
            List.filter isNice1 lines
                |> List.length
                |> (\len -> String.fromInt len ++ " strings are nice")
                |> Ok

        repeatedWithOneBetween =
            Regex.fromString "(.).\\1"
                |> Maybe.withDefault Regex.never

        repeatedPair =
            Regex.fromString "(.)(.).*\\1\\2"
                |> Maybe.withDefault Regex.never

        isNice2 str =
            Regex.contains repeatedPair str
                && Regex.contains repeatedWithOneBetween str

        r2 =
            List.filter isNice2 lines
                |> List.length
                |> (\len -> String.fromInt len ++ " strings are nice")
                |> Ok
    in
    ( r1
    , r2
    )


