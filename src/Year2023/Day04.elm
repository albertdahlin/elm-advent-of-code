module Year2023.Day04 exposing (..)

import Dict exposing (Dict)
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Set exposing (Set)
import Util.Parser


solution =
    { solve = solve
    , title = "Scratchcards"
    , subtitle = "Help the Elf to figure out what he has won."
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Card =
    { winning : Set Int
    , played : Set Int
    }


parseInput : String -> Result String (List Card)
parseInput input =
    let
        setOfNumbers =
            Util.Parser.listWhile
                (Parser.succeed identity
                    |= Util.Parser.int
                    |. Parser.spaces
                )
                |> Parser.map Set.fromList
    in
    Util.Parser.run
        (Util.Parser.lines
            (Parser.succeed Card
                |. Parser.token "Card"
                |. Parser.spaces
                |. Parser.int
                |. Parser.spaces
                |. Parser.symbol ":"
                |. Parser.spaces
                |= setOfNumbers
                |. Parser.spaces
                |. Parser.symbol "|"
                |. Parser.spaces
                |= setOfNumbers
            )
        )
        input


solve1 : List Card -> String
solve1 cards =
    cards
        |> List.map
            (\card ->
                Set.intersect card.winning card.played
                    |> Set.size
                    |> (\s -> floor (2 ^ (s - 1) |> toFloat))
            )
        |> List.sum
        |> String.fromInt


solve2 : List Card -> String
solve2 cards =
    let
        totalCards =
            List.length cards
    in
    cards
        |> List.Extra.indexedFoldl
            (\cardIdx card countByCard ->
                let
                    winCount =
                        Set.intersect card.winning card.played
                            |> Set.size

                    copiesCount =
                        Dict.get cardIdx countByCard
                            |> Maybe.withDefault 1
                in
                List.range
                    (cardIdx + 1)
                    (cardIdx + winCount |> min totalCards)
                    |> List.foldl
                        (\cardId ->
                            Dict.update
                                cardId
                                (Maybe.withDefault 1
                                    >> (+) copiesCount
                                    >> Just
                                )
                        )
                        (Dict.insert cardIdx copiesCount countByCard)
            )
            Dict.empty
        |> Dict.values
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
