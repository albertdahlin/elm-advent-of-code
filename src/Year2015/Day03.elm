module Year2015.Day03 exposing (..)

import Performance exposing (Performance)
import List.Extra as List
import Result.Extra as Result
import Set exposing (Set)
import Util.Vec2 as Vec2


solution =
    { solve = solve
    , title = "Perfectly Spherical Houses in a Vacuum"
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        locations =
            input
                |> String.toList
                |> List.map
                    (\c ->
                        case c of
                            '^' ->
                                Ok ( 0, 1 )

                            'v' ->
                                Ok ( 0, -1 )

                            '>' ->
                                Ok ( 1, 0 )

                            '<' ->
                                Ok ( -1, 0 )

                            _ ->
                                Err ("Not a direction" ++ String.fromChar c)
                    )
                |> Result.combine

        r1 =
            locations
                |> Result.map
                    (List.scanl Vec2.add ( 0, 0 )
                        >> Set.fromList
                        >> Set.size
                        >> (\len -> "Santa visits " ++ String.fromInt len ++ " unique houses")
                    )

        r2 =
            locations
                |> Result.map
                    (List.foldl
                        (\vec state ->
                            if state.toSanta then
                                let
                                    newPos =
                                        Vec2.add state.santaPos vec
                                in
                                { state
                                    | santaPos = newPos
                                    , visited = Set.insert newPos state.visited
                                    , toSanta = False
                                }

                            else
                                let
                                    newPos =
                                        Vec2.add state.roboPos vec
                                in
                                { state
                                    | roboPos = newPos
                                    , visited = Set.insert newPos state.visited
                                    , toSanta = True
                                }
                        )
                        { santaPos = ( 0, 0 )
                        , roboPos = ( 0, 0 )
                        , visited = Set.singleton ( 0, 0 )
                        , toSanta = True
                        }
                        >> .visited
                        >> Set.size
                        >> (\len -> "Santa and Robo visits " ++ String.fromInt len ++ " unique houses")
                    )
    in
    ( r1
    , r2
    )
