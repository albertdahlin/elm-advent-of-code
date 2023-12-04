module Year2015.Day11 exposing (..)

import Array exposing (Array)
import Performance exposing (Performance)
import Util.Char
import Util.Digits as Digits exposing (Digits(..))
import Util.Loop


solution =
    { solve = solve
    , title = "Corporate Policy"
    , subtitle = "Find the next valid password according to the security rules"
    , tests = []
    , performance = Performance.Acceptable
    }


prohibitedLetters : List Int
prohibitedLetters =
    [ 'i', 'o', 'l' ]
        |> List.map Util.Char.toAlphaIndex


containsValidCharacters : List Int -> Bool
containsValidCharacters list =
    case list of
        [] ->
            True

        n :: rest ->
            if List.member n prohibitedLetters then
                False

            else
                containsValidCharacters rest


containsPairs : List Int -> Bool
containsPairs digits =
    case digits of
        [] ->
            False

        n :: rest ->
            List.foldl
                (\digit ( prev, count ) ->
                    if digit == prev then
                        -- Digit is >= 0, this makes sure we don't match triplets, e.g x, 2, 2, 2
                        ( -1
                        , count + 1
                        )

                    else
                        ( digit
                        , count
                        )
                )
                ( n, 0 )
                rest
                |> Tuple.second
                |> (<=) 2


containsStraight : List Int -> Bool
containsStraight list =
    case list of
        n1 :: n2 :: n3 :: rest ->
            if n1 == n2 + 1 && n1 == n3 + 2 then
                True

            else
                containsStraight (n2 :: n3 :: rest)

        _ ->
            False


isPasswordValid : Digits -> Bool
isPasswordValid (Digits _ list) =
    containsPairs list
        && containsStraight list
        && containsValidCharacters list


nextPassword : Digits -> Digits
nextPassword pw =
    Digits.add 1 pw


passwordToString : Digits -> String
passwordToString digits =
    digits
        |> Digits.toList
        |> List.map Util.Char.fromAlphaIndex
        |> String.fromList
        |> String.toLower


solve : String -> ( Result String String, Result String String )
solve input =
    let
        password =
            input
                |> String.toUpper
                |> String.toList
                |> List.map Util.Char.toAlphaIndex
                |> Digits.fromListWithBase 26

        pw1 =
            password
                |> Util.Loop.repeatUntil isPasswordValid nextPassword

        pw2 =
            pw1
                |> nextPassword
                |> Util.Loop.repeatUntil isPasswordValid nextPassword
    in
    ( Ok (passwordToString pw1)
    , Ok (passwordToString pw2)
    )
