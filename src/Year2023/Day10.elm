module Year2023.Day10 exposing (..)

import Dict exposing (Dict)
import Performance
import Set exposing (Set)
import Util.Dict as Dict
import Util.Grid as Grid exposing (Dir(..), Turn(..))
import Util.List as List
import Util.Tuple as Tuple


solution =
    { solve = solve
    , title = "Pipe Maze"
    , subtitle = "Find the animal in a loop of pipes"
    , tests = []
    , performance = Performance.Acceptable
    }


parseInput : String -> Grid
parseInput =
    Grid.parse
        (\char ->
            if char == '.' then
                Nothing

            else
                Just char
        )


type alias Grid =
    Grid.Grid Char


charDirection : Dir -> Char -> Maybe Dir
charDirection going char =
    case ( going, char ) of
        ( Right, 'J' ) ->
            Just Up

        ( Down, 'J' ) ->
            Just Left

        ( Right, '7' ) ->
            Just Down

        ( Up, '7' ) ->
            Just Left

        ( Right, '-' ) ->
            Just Right

        ( Down, '|' ) ->
            Just Down

        ( Down, 'L' ) ->
            Just Right

        ( Left, '-' ) ->
            Just Left

        ( Left, 'F' ) ->
            Just Down

        ( Left, 'L' ) ->
            Just Up

        ( Up, '|' ) ->
            Just Up

        ( Up, 'F' ) ->
            Just Right

        _ ->
            Nothing


tryDirection : Grid -> ( Int, Int ) -> Dir -> Maybe ( ( Int, Int ), Dir )
tryDirection grid from dir =
    let
        next =
            Grid.rowColFrom dir
                |> Tuple.add from
    in
    Dict.get next grid.cells
        |> Maybe.andThen (charDirection dir)
        |> Maybe.map (Tuple.pair next)


walk :
    Grid
    -> Int
    -> ( ( Int, Int ), Dir )
    -> Dict ( Int, Int ) Int
    -> Dict ( Int, Int ) Int
walk grid count ( pos1, dir1 ) visited =
    let
        newVis =
            Dict.insert pos1 count visited
    in
    case tryDirection grid pos1 dir1 of
        Just next ->
            walk grid (count + 1) next newVis

        Nothing ->
            newVis


getInsidePoints : Grid -> Dict ( Int, Int ) a -> Set ( Int, Int )
getInsidePoints grid path =
    Grid.fold
        (\pos mbChar ( insidePoints, isInside, prevChar ) ->
            if Dict.member pos path then
                case mbChar of
                    Just '|' ->
                        ( insidePoints
                        , not isInside
                        , '|'
                        )

                    Just 'F' ->
                        ( insidePoints
                        , isInside
                        , 'F'
                        )

                    Just 'J' ->
                        ( insidePoints
                        , xor (prevChar == 'F') isInside
                        , 'J'
                        )

                    Just 'L' ->
                        ( insidePoints
                        , isInside
                        , 'L'
                        )

                    Just '7' ->
                        ( insidePoints
                        , xor (prevChar == 'L') isInside
                        , '7'
                        )

                    _ ->
                        ( insidePoints
                        , isInside
                        , prevChar
                        )

            else
                ( if isInside then
                    Set.insert pos insidePoints

                  else
                    insidePoints
                , isInside
                , prevChar
                )
        )
        ( Set.empty
        , False
        , ' '
        )
        grid
        |> (\( s, _, _ ) -> s)


getStartPointChar : Dir -> Dir -> Char
getStartPointChar dir1 dir2 =
    case ( dir1, dir2 ) of
        ( Down, Right ) ->
            'F'

        ( Down, Left ) ->
            '7'

        ( Up, Left ) ->
            'J'

        ( Up, Right ) ->
            'L'

        ( Up, Down ) ->
            '|'

        ( Left, Right ) ->
            '-'

        _ ->
            'S'


solve : String -> ( Result String String, Result String String )
solve string =
    let
        grid =
            parseInput string

        startPoint =
            Dict.filter (\_ char -> char == 'S') grid.cells
                |> Dict.toList
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault ( 0, 0 )

        ( firstDir, secondDir ) =
            [ Up, Down, Left, Right ]
                |> List.filterMap
                    (\dir ->
                        tryDirection grid startPoint dir
                            |> Maybe.map (always dir)
                    )
                |> List.when2 Tuple.pair
                |> Maybe.withDefault ( Up, Down )

        firstPath =
            walk grid 0 ( startPoint, firstDir ) Dict.empty

        secondPath =
            walk grid 0 ( startPoint, secondDir ) Dict.empty

        path =
            Dict.aggregateUnion min firstPath secondPath

        startPointChar =
            getStartPointChar firstDir secondDir

        newGrid =
            { grid | cells = Dict.insert startPoint startPointChar grid.cells }

        insidePoints =
            getInsidePoints newGrid path
    in
    ( path
        |> Dict.foldl (\_ -> max) 0
        |> String.fromInt
        |> Ok
    , Set.size insidePoints
        |> String.fromInt
        |> Ok
    )
