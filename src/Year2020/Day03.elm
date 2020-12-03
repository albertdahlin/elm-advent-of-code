module Year2020.Day03 exposing (..)

import Array exposing (Array)
import Performance exposing (Performance)
import Set exposing (Set)
import Util.Vec2 exposing (Vec2)


solution =
    { solve = solve
    , title = "Toboggan Trajectory"
    , subtitle = "Figure out how many trees will occur in your path."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        lines : List String
        lines =
            String.lines input

        rowsOfIndiciesWhereTreesOccur : Array (Set Int)
        rowsOfIndiciesWhereTreesOccur =
            lines
                |> List.map
                    (String.toList
                        >> List.foldl
                            (\char ( idx, set ) ->
                                case char of
                                    '#' ->
                                        ( idx + 1
                                        , Set.insert idx set
                                        )

                                    _ ->
                                        ( idx + 1
                                        , set
                                        )
                            )
                            ( 0, Set.empty )
                        >> Tuple.second
                    )
                |> Array.fromList

        rowLength : Int
        rowLength =
            List.head lines
                |> Maybe.map String.length
                |> Maybe.withDefault 0

        countTreesFrom : Vec2 Int -> Vec2 Int -> Int
        countTreesFrom ( rowPos, colPos ) ( rowInc, colInc ) =
            case Array.get rowPos rowsOfIndiciesWhereTreesOccur of
                Just indicesWhereTreesOccur ->
                    if Set.member colPos indicesWhereTreesOccur then
                        1
                            + countTreesFrom
                                ( rowPos + rowInc, colPos + colInc |> modBy rowLength )
                                ( rowInc, colInc )

                    else
                        countTreesFrom
                            ( rowPos + rowInc, colPos + colInc |> modBy rowLength )
                            ( rowInc, colInc )

                Nothing ->
                    0

        r1 =
            countTreesFrom ( 0, 0 ) ( 1, 3 )
                |> (\n -> "Encountered " ++ String.fromInt n ++ " trees")
                |> Ok

        r2 =
            [ countTreesFrom ( 0, 0 ) ( 1, 1 )
            , countTreesFrom ( 0, 0 ) ( 1, 3 )
            , countTreesFrom ( 0, 0 ) ( 1, 5 )
            , countTreesFrom ( 0, 0 ) ( 1, 7 )
            , countTreesFrom ( 0, 0 ) ( 2, 1 )
            ]
                |> List.product
                |> (\n -> "Product is " ++ String.fromInt n)
                |> Ok
    in
    ( r1
    , r2
    )
