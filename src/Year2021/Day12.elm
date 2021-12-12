module Year2021.Day12 exposing (..)

import Dict exposing (Dict)
import Performance exposing (Performance)
import Result.Extra as Result
import Set exposing (Set)
import Util.Dict


solution =
    { solve = solve
    , title = "Passage Pathing"
    , subtitle = "Search for all paths through the subterranean subsystem."
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Graph =
    Dict String (List String)


solve : String -> ( Result String String, Result String String )
solve input =
    let
        parsedGraph =
            parse input

        r1 =
            parsedGraph
                |> Result.map
                    (\graph ->
                        findPaths1 "start" [] graph
                            |> List.length
                            |> String.fromInt
                    )

        r2 =
            parsedGraph
                |> Result.map
                    (\graph ->
                        findPaths2 False "start" [] graph
                            |> Set.fromList
                            |> Set.size
                            |> String.fromInt
                    )
    in
    ( r1
    , r2
    )


findPaths1 : String -> List String -> Graph -> List (List String)
findPaths1 node path graph =
    case Dict.get node graph of
        Just possibleNodes ->
            let
                graphWithoutNode =
                    if isSmall node then
                        Dict.remove node graph

                    else
                        graph
            in
            if node == "end" then
                [ node :: path ]

            else
                List.concatMap
                    (\next ->
                        findPaths1
                            next
                            (node :: path)
                            graphWithoutNode
                    )
                    possibleNodes

        Nothing ->
            []


findPaths2 : Bool -> String -> List String -> Graph -> List (List String)
findPaths2 isTwiceUsed node path graph =
    case Dict.get node graph of
        Just possibleNodes ->
            let
                graphWithoutNode =
                    if isSmall node then
                        Dict.remove node graph

                    else
                        graph
            in
            if node == "end" then
                [ node :: path ]

            else if isTwiceUsed || node == "start" then
                List.concatMap
                    (\next ->
                        findPaths2
                            isTwiceUsed
                            next
                            (node :: path)
                            graphWithoutNode
                    )
                    possibleNodes

            else if isSmall node then
                List.concatMap
                    (\next ->
                        findPaths2
                            True
                            next
                            (node :: path)
                            graph
                            ++ findPaths2
                                False
                                next
                                (node :: path)
                                graphWithoutNode
                    )
                    possibleNodes

            else
                List.concatMap
                    (\next ->
                        findPaths2
                            isTwiceUsed
                            next
                            (node :: path)
                            graph
                    )
                    possibleNodes

        Nothing ->
            []


isSmall : String -> Bool
isSmall n =
    String.uncons n
        |> Maybe.map (Tuple.first >> Char.isLower)
        |> Maybe.withDefault False



-- PARSE


parse : String -> Result String Graph
parse =
    String.lines
        >> List.map
            (\line ->
                case String.split "-" line of
                    [ a, b ] ->
                        Ok ( a, b )

                    _ ->
                        Err "Line is not formated correctly"
            )
        >> Result.combine
        >> Result.map
            (List.foldl addTo Dict.empty
                >> Dict.map (\_ -> Set.toList)
            )


addTo : ( String, String ) -> Dict String (Set String) -> Dict String (Set String)
addTo ( n1, n2 ) dict =
    Util.Dict.addToSet n1 n2 dict
        |> Util.Dict.addToSet n2 n1
