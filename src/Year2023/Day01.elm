module Year2023.Day01 exposing (..)

import Dict exposing (Dict)
import List.Extra as List
import Performance exposing (Performance)
import Regex exposing (Regex)
import Result.Extra as Result


solution =
    { solve = solve
    , title = "Trebuchet?!"
    , subtitle = ""
    , tests = []
    , performance = Performance.Acceptable
    }


words : List String
words =
    [ "zero"
    , "one"
    , "two"
    , "three"
    , "four"
    , "five"
    , "six"
    , "seven"
    , "eight"
    , "nine"
    ]


firstNumber : Regex
firstNumber =
    ("(\\d|" ++ String.join "|" words ++ ").*")
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


lastNumber : Regex
lastNumber =
    (".*(\\d|" ++ String.join "|" words ++ ")")
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


wordToIntTable : Dict String Int
wordToIntTable =
    words
        |> List.indexedMap (\i w -> ( w, i ))
        |> Dict.fromList


wordToInt : String -> Maybe Int
wordToInt word =
    case String.toInt word of
        Just i ->
            Just i

        Nothing ->
            Dict.get word wordToIntTable


expectOne : List Regex.Match -> Maybe String
expectOne matches =
    case matches of
        [ match ] ->
            case match.submatches of
                [ submatch ] ->
                    submatch

                _ ->
                    Nothing

        _ ->
            Nothing


find : Regex -> String -> Maybe Int
find regex haystack =
    Regex.find regex haystack
        |> expectOne
        |> Maybe.andThen wordToInt


makeNumber : Int -> Int -> Int
makeNumber first last =
    10 * first + last


solve : String -> ( Result String String, Result String String )
solve input =
    let
        r1 =
            String.lines input
                |> List.map
                    (\line ->
                        let
                            numbers =
                                String.split "" line
                                    |> List.filterMap String.toInt
                        in
                        Maybe.map2 makeNumber
                            (List.head numbers)
                            (List.last numbers)
                            |> Result.fromMaybe "No number found"
                    )
                |> Result.combine
                |> Result.map (List.sum >> String.fromInt)

        r2 =
            String.lines input
                |> List.map
                    (\line ->
                        Maybe.map2 makeNumber
                            (find firstNumber line)
                            (find lastNumber line)
                            |> Result.fromMaybe "No number found"
                    )
                |> Result.combine
                |> Result.map (List.sum >> String.fromInt)
    in
    ( r1
    , r2
    )
