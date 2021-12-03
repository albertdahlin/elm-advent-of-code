module Year2015.Day09 exposing (..)

import Dict exposing (Dict)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Result.Extra as Result
import Set exposing (Set)
import Util.Parser


solution =
    { solve = solve
    , title = "All in a Single Night"
    , subtitle = "Find the shortest and longest path."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        distances =
            input
                |> Util.Parser.run (Util.Parser.parseRowsUsing rowParser)
                |> Result.map Dict.fromList

        allLocations =
            distances
                |> Result.map
                    (Dict.foldl
                        (\( l1, l2 ) _ ->
                            Set.insert l1
                                >> Set.insert l2
                        )
                        Set.empty
                        >> Set.toList
                    )

        shortest =
            Result.map2 findMin distances allLocations

        longest =
            Result.map2 findMax distances allLocations

        r1 =
            Result.map String.fromInt shortest

        r2 =
            Result.map String.fromInt longest
    in
    ( r1
    , r2
    )


findMin : Dict ( String, String ) Int -> List String -> Int
findMin distances list =
    List.permutations list
        |> List.foldl
            (\path ->
                min (pathLength distances path)
            )
            999999


findMax : Dict ( String, String ) Int -> List String -> Int
findMax distances list =
    List.permutations list
        |> List.foldl
            (\path val ->
                max (pathLength distances path) val
            )
            0


pathLength : Dict ( String, String ) Int -> List String -> Int
pathLength distances list =
    case list of
        [] ->
            0

        x :: xs ->
            pathLengthHelp distances x xs


pathLengthHelp : Dict ( String, String ) Int -> String -> List String -> Int
pathLengthHelp distances prev list =
    case list of
        [] ->
            0

        x :: xs ->
            let
                dist =
                    Dict.get
                        (sortKey prev x)
                        distances
                        |> Maybe.withDefault 0
            in
            dist + pathLengthHelp distances x xs


sortKey : String -> String -> ( String, String )
sortKey l1 l2 =
    if l1 < l2 then
        ( l1, l2 )

    else
        ( l2, l1 )


rowParser : Parser ( ( String, String ), Int )
rowParser =
    Parser.succeed
        (\loc1 loc2 distance ->
            ( sortKey loc1 loc2, distance )
        )
        |= Util.Parser.alpha
        |. Parser.spaces
        |. Parser.keyword "to"
        |. Parser.spaces
        |= Util.Parser.alpha
        |. Parser.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= Parser.int
