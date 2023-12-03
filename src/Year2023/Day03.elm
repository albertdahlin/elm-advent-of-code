module Year2023.Day03 exposing (..)

import Dict exposing (Dict)
import List.Extra
import Performance exposing (Performance)
import Regex exposing (Regex)


solution =
    { solve = solve
    , title = "Gear Ratios"
    , subtitle = "Parse a schematic grid and figure out some gear ratios."
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Grids =
    { symbols : Dict ( Int, Int ) Char
    , numbers : Dict ( Int, Int ) Int
    }


numbersRegex : Regex
numbersRegex =
    Regex.fromString "(\\d+)"
        |> Maybe.withDefault Regex.never


parseSymbols : Int -> String -> Dict ( Int, Int ) Char
parseSymbols row line =
    String.toList line
        |> List.foldl
            (\char ( symbols, col ) ->
                ( if Char.isDigit char || char == '.' then
                    symbols

                  else
                    Dict.insert
                        ( row, col )
                        char
                        symbols
                , col + 1
                )
            )
            ( Dict.empty
            , 0
            )
        |> Tuple.first


parseNumbers : Int -> String -> Dict ( Int, Int ) Int
parseNumbers row line =
    Regex.find numbersRegex line
        |> List.foldl
            (\match numbers ->
                let
                    n =
                        String.toInt match.match
                            -- Must be a number or regex wouldn't match
                            |> Maybe.withDefault -100
                in
                List.range
                    match.index
                    (match.index + String.length match.match - 1)
                    |> List.foldl
                        (\col grid ->
                            Dict.insert
                                ( row, col )
                                n
                                grid
                        )
                        numbers
            )
            Dict.empty


parseInput : String -> Grids
parseInput input =
    let
        ( symbols, numbers, _ ) =
            String.lines input
                |> List.foldl
                    (\line ( symbols1, numbers1, row ) ->
                        ( parseSymbols row line
                            |> Dict.union symbols1
                        , parseNumbers row line
                            |> Dict.union numbers1
                        , row + 1
                        )
                    )
                    ( Dict.empty
                    , Dict.empty
                    , 0
                    )
    in
    { numbers = numbers
    , symbols = symbols
    }


getAdjacent : ( Int, Int ) -> Dict ( Int, Int ) a -> List a
getAdjacent ( row, col ) dict =
    [ Dict.get ( row - 1, col - 1 ) dict
    , Dict.get ( row - 1, col ) dict
    , Dict.get ( row - 1, col + 1 ) dict
    , Dict.get ( row, col - 1 ) dict
    , Dict.get ( row, col + 1 ) dict
    , Dict.get ( row + 1, col - 1 ) dict
    , Dict.get ( row + 1, col ) dict
    , Dict.get ( row + 1, col + 1 ) dict
    ]
        |> List.filterMap identity


solve1 : Grids -> String
solve1 grid =
    grid.symbols
        |> Dict.map
            (\pos sym ->
                getAdjacent pos grid.numbers
                    |> List.Extra.unique
                    |> List.sum
            )
        |> Dict.values
        |> List.sum
        |> String.fromInt


solve2 : Grids -> String
solve2 grid =
    grid.symbols
        |> Dict.filter (\_ char -> char == '*')
        |> Dict.map
            (\pos sym ->
                case getAdjacent pos grid.numbers |> List.Extra.unique of
                    [ first, second ] ->
                        first * second

                    _ ->
                        0
            )
        |> Dict.values
        |> List.sum
        |> String.fromInt


solve : String -> ( Result String String, Result String String )
solve input =
    let
        parsedInput =
            parseInput input
    in
    ( solve1 parsedInput
        |> Ok
    , solve2 parsedInput
        |> Ok
    )
