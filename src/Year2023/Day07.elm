module Year2023.Day07 exposing (..)

import Dict exposing (Dict)
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Util.Parser


solution =
    { solve = solve
    , title = "Camel Cards"
    , subtitle = "Calculate the total winnings of a series of camel card games."
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Hand =
    { cards : List Char
    , bid : Int
    }


parseInput : String -> Result String (List Hand)
parseInput input =
    Util.Parser.run
        (Util.Parser.lines
            (Parser.succeed Hand
                |= (Util.Parser.nonEmptyStringWhile ((/=) ' ')
                        |> Parser.map String.toList
                   )
                |. Parser.spaces
                |= Parser.int
            )
        )
        input


{-| Card values, higher is better.

Value zero is reserved for the Joker card

-}
cardValues : Dict Char Int
cardValues =
    [ 'A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2' ]
        |> List.reverse
        |> List.indexedMap (\i card -> ( card, i + 1 ))
        |> Dict.fromList


{-| Higher is better
-}
cardValue : Char -> Int
cardValue card =
    Dict.get card cardValues
        |> Maybe.withDefault -100


{-| Hand type rank from a frequency list. Higher is better
-}
typeFromFrequencies : List Int -> Int
typeFromFrequencies freq =
    case List.sort freq of
        [ 5 ] ->
            -- Five cards
            6

        [ 1, 4 ] ->
            -- Four cards
            5

        [ 2, 3 ] ->
            -- Full House
            4

        [ 1, 1, 3 ] ->
            -- Tree of a kind
            3

        [ 1, 2, 2 ] ->
            -- Two pairs
            2

        [ 1, 1, 1, 2 ] ->
            -- One pairs
            1

        [ 1, 1, 1, 1, 1 ] ->
            -- High card
            0

        _ ->
            -100


{-| Compare hands, first by hand type and if equal, by best card
-}
compareHandsUsing :
    (List Int -> comparable)
    -> ( List Int, x )
    -> ( List Int, x )
    -> Order
compareHandsUsing toHandType ( h1, _ ) ( h2, _ ) =
    -- Compare hand types
    case compare (toHandType h1) (toHandType h2) of
        GT ->
            GT

        LT ->
            LT

        EQ ->
            -- Compare cards if hand types are equal
            compare h1 h2



-- PART 1


{-| Hand type rank, higher is better
-}
handType : List Int -> Int
handType hand =
    hand
        |> List.Extra.frequencies
        |> List.map Tuple.second
        |> typeFromFrequencies


solve1 : List Hand -> String
solve1 input =
    input
        |> List.map
            (\hand ->
                ( List.map cardValue hand.cards
                , hand.bid
                )
            )
        |> List.sortWith (compareHandsUsing handType)
        |> List.indexedMap (\rank ( _, bid ) -> (rank + 1) * bid)
        |> List.sum
        |> String.fromInt



-- PART 2


{-| Hand type rank with joker, higher is better

Remove the joker but add its frequency to the most common card.

-}
handTypeWithJoker : List Int -> Int
handTypeWithJoker hand =
    let
        frequencies =
            List.Extra.frequencies hand
                |> List.sort

        jokerAddedToMostCommonCard =
            case frequencies of
                -- Zero is the joker
                ( 0, jokerFreq ) :: otherCards ->
                    case List.sortBy Tuple.second otherCards |> List.reverse of
                        ( mostCommonCard, freq ) :: cards ->
                            ( mostCommonCard, freq + jokerFreq ) :: cards

                        _ ->
                            frequencies

                _ ->
                    frequencies
    in
    List.map Tuple.second jokerAddedToMostCommonCard
        |> typeFromFrequencies


{-| Higher is better
-}
cardValueWithJoker : Char -> Int
cardValueWithJoker card =
    if card == 'J' then
        0

    else
        cardValue card


solve2 : List Hand -> String
solve2 input =
    input
        |> List.map
            (\hand ->
                ( List.map cardValueWithJoker hand.cards
                , hand.bid
                )
            )
        |> List.sortWith (compareHandsUsing handTypeWithJoker)
        |> List.indexedMap (\rank ( _, bid ) -> (rank + 1) * bid)
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
