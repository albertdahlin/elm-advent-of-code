module Year2020.Day13 exposing (..)

import List.Extra as List
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Result.Extra as Result
import Util.Parser
import Util.Vec2 as Vec2 exposing (Vec2)


solution =
    { solve = solve
    , title = ""
    , subtitle = ""
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        ( wait, numbers ) =
            case String.lines input of
                [ fst, snd ] ->
                    case String.toInt fst of
                        Just n ->
                            ( n
                            , String.split "," snd
                                |> List.indexedMap
                                    (\idx c ->
                                        case String.toInt c of
                                            Just id ->
                                                Just ( idx, id )

                                            Nothing ->
                                                Nothing
                                    )
                                |> List.filterMap identity
                            )

                        Nothing ->
                            ( 0, [] )

                _ ->
                    ( 0, [] )

        r1 =
            numbers
                |> List.map
                    (\( _, n ) ->
                        ( if modBy n wait == 0 then
                            0

                          else
                            n * (wait // n + 1) - wait
                        , n
                        )
                    )
                |> List.minimumBy Tuple.first
                |> Maybe.map
                    (\( a, b ) -> a * b)
                |> Result.fromMaybe "No solution"
                |> Result.map String.fromInt

        r2 =
            Err "Not implemented"
    in
    ( r1
    , r2
    )

{-

(0,7),(1,13),(4,59),(6,31),(7,19)


n = 7a = 13b = 59c = 31d = 19e = 3162341x

7a = 13b - 1
7a = 59c - 4
7a = 31d - 6
7a = 19e = 133



7a = 13b - 1 = 59c - 4 = 31d - 6 = 19e - 7


7a + 1 = 13b


3a = 4b = 3 * 4 = 4 * 3 = 12

3a = 4b - 1
ax = by - k

a*b - (b-k)a

3 * 5 = 15
3 * 5 - (5-1)*3

3x = 5y - 1

((5 - 3n) mod 5 = k



0   3       5
1
2
3   3
4       4
5           5
6   3
7
8       4
9   3
10          5
11
12  3   4
13
14
15  3       5
-}
