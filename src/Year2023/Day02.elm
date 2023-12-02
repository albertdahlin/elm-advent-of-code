module Year2023.Day02 exposing (..)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Result.Extra as Result
import Util.Parser


solution =
    { solve = solve
    , title = "Cube Conundrum"
    , subtitle = "Figure out some information about cubes in a bag."
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Game =
    { id : Int
    , boxes : List ( Int, String )
    }


parseInput : String -> Result String (List Game)
parseInput =
    Util.Parser.run
        (Parser.succeed Game
            |. Parser.token "Game"
            |. Parser.spaces
            |= Util.Parser.int
            |. Parser.symbol ":"
            |. Parser.spaces
            |= Util.Parser.listSeparatedBy
                (Util.Parser.atLeastOneOf [ ';', ',', ' ' ])
                (Parser.succeed Tuple.pair
                    |= Util.Parser.int
                    |. Parser.spaces
                    |= Util.Parser.alpha
                )
            |> Util.Parser.lines
        )


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
    in
    ( Result.map solve1 parsedInput
    , Result.map solve2 parsedInput
    )
