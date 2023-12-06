module Year2023.Day05 exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Set exposing (Set)
import Util.Parser


solution =
    { solve = solve
    , title = "If You Give A Seed A Fertilizer"
    , subtitle = "Help the Elves with their food production problem"
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Almanac =
    { seeds : List Int
    , maps : List Map
    }


type alias Map =
    { name : String
    , rows : List Row
    }


type alias Row =
    { from : Int
    , to : Int
    , diff : Int
    }


parseInput : String -> Result String Almanac
parseInput input =
    Util.Parser.run
        (Parser.succeed Almanac
            |. Parser.token "seeds"
            |. Parser.symbol ":"
            |. Parser.spaces
            |= Util.Parser.listWhile
                (Util.Parser.int
                    |. Parser.spaces
                )
            |= Util.Parser.listWhile
                (Parser.succeed
                    (\name rows ->
                        { name = name
                        , rows = List.sortBy (\r -> ( r.from, r.to )) rows
                        }
                    )
                    |= (Parser.succeed ()
                            |. Parser.chompWhile (\c -> c /= ':')
                            |> Parser.getChompedString
                       )
                    |. Parser.symbol ":"
                    |. Parser.spaces
                    |= Util.Parser.listWhile
                        (Parser.succeed
                            (\dest src len ->
                                -- It's more convenient with ranges.
                                { from = src
                                , to = src + len - 1
                                , diff = dest - src
                                }
                            )
                            |= Util.Parser.int
                            |. Parser.spaces
                            |= Util.Parser.int
                            |. Parser.spaces
                            |= Util.Parser.int
                            |. Parser.spaces
                        )
                    |. Parser.spaces
                )
        )
        input



-- PART 1


mapSeed : List Row -> Int -> Int
mapSeed list val =
    case list of
        [] ->
            val

        row :: rest ->
            if val >= row.from && val <= row.to then
                val + row.diff

            else
                mapSeed rest val


solve1 : Almanac -> String
solve1 input =
    input.seeds
        |> List.map
            (\seed ->
                List.foldl
                    (\map -> mapSeed map.rows)
                    seed
                    input.maps
            )
        |> List.sort
        |> List.head
        |> Maybe.withDefault -1
        |> String.fromInt



-- PART 2


seedRangesFrom : List Int -> List ( Int, Int )
seedRangesFrom list =
    case list of
        seedValue :: length :: rest ->
            ( seedValue
            , seedValue + length - 1
            )
                :: seedRangesFrom rest

        _ ->
            []


convertRanges : List Row -> List ( Int, Int ) -> List ( Int, Int )
convertRanges rows ranges =
    case ranges of
        [] ->
            []

        ( min, max ) :: nextRanges ->
            case rows of
                [] ->
                    ranges

                row :: nextRows ->
                    if max < row.from then
                        {- Before, no overlap: min < from && max < from
                           ----------|from-to|-  <- row in map
                           -|min-max|----------  <- current range
                        -}
                        ( min, max ) :: convertRanges rows nextRanges

                    else if min < row.from then
                        {- Before, with overlap: min < from && max >= from
                           -----|from-to|-
                           -|min-max|-----
                        -}
                        ( min, row.from - 1 )
                            :: convertRanges
                                rows
                                (( row.from, max ) :: nextRanges)

                    else if max <= row.to then
                        {- Inside: max <= to && min >= from
                           -|from-----to|-
                           ---|min-max|---
                        -}
                        ( min + row.diff, max + row.diff )
                            :: convertRanges
                                rows
                                nextRanges

                    else if min <= row.to then
                        {- After, with overlap: min <= to && max > to
                           -|from-to|-----
                           ----|min-max|-
                        -}
                        ( min + row.diff, row.to + row.diff )
                            :: convertRanges
                                nextRows
                                (( row.to + 1, max ) :: nextRanges)

                    else
                        {- After, no overlap: min > to
                           -|from-to|----------
                           ----------|min-max|-
                        -}
                        convertRanges nextRows ranges


solve2 : Almanac -> String
solve2 input =
    List.foldl
        (\map ranges ->
            convertRanges map.rows ranges
                |> List.sort
        )
        (seedRangesFrom input.seeds)
        input.maps
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault -1
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
