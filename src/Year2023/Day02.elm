module Year2023.Day02 exposing (..)

import Dict exposing (Dict)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Result.Extra as Result
import Util.Parser
import Util.Regex


solution =
    { solve = solve
    , title = "Cube Conundrum"
    , subtitle = ""
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Game =
    { id : Int
    , boxes : List ( Int, String )
    }


parseInput : String -> Result String (List Game)
parseInput input =
    String.lines input
        |> List.map
            (\line ->
                Result.map2 Game
                    (Util.Regex.parseInt "Game (\\d+)" line)
                    (Util.Regex.parsePairs "(\\d+) (\\w+)" line
                        |> Result.andThen
                            (List.map
                                (\( qtyString, color ) ->
                                    String.toInt qtyString
                                        |> Result.fromMaybe "Not a number"
                                        |> Result.map (\qty -> ( qty, color ))
                                )
                                >> Result.combine
                            )
                    )
            )
        |> Result.combine


solve1 : List Game -> String
solve1 games =
    let
        isNotPossible ( qty, color ) =
            (color == "red" && qty > 12)
                || (color == "green" && qty > 13)
                || (color == "blue" && qty > 14)
    in
    List.map
        (\game ->
            if List.any isNotPossible game.boxes then
                0

            else
                game.id
        )
        games
        |> List.sum
        |> String.fromInt


solve2 : List Game -> String
solve2 games =
    List.map
        (\game ->
            game.boxes
                |> List.foldl
                    (\( qty, color ) largestByColor ->
                        let
                            largestSoFar =
                                Dict.get color largestByColor
                                    |> Maybe.withDefault 0
                        in
                        Dict.insert
                            color
                            (max largestSoFar qty)
                            largestByColor
                    )
                    Dict.empty
                |> Dict.values
                |> List.product
        )
        games
        |> List.sum
        |> String.fromInt


solve : String -> ( Result String String, Result String String )
solve input =
    let
        parsedInput =
            parseInput input

        part1 =
            parsedInput
                |> Result.map solve1

        part2 =
            parsedInput
                |> Result.map solve2
    in
    ( part1
    , part2
    )
