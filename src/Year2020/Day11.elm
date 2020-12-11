module Year2020.Day11 exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import List.Extra as List
import Performance exposing (Performance)
import Result.Extra as Result
import Util.Grid as Grid exposing (Grid)
import Util.Loop as Loop


solution =
    { solve = solve
    , title = "Seating System"
    , subtitle = "Solve \"Game of Life\" on the ferry seats."
    , tests = []
    , performance = Performance.Acceptable
    }


type Cell
    = Occupied
    | Free
    | Floor


solve : String -> ( Result String String, Result String String )
solve input =
    let
        result =
            String.lines input
                |> List.map
                    (String.toList
                        >> List.map charToCell
                        >> Result.combine
                        >> Result.map Array.fromList
                    )
                |> Result.combine
                |> Result.andThen
                    (Array.fromList
                        >> Grid.fromRows
                        >> Result.fromMaybe "Mismatching row length"
                    )

        r1 =
            Result.map
                (\grid ->
                    Loop.untilNoChangeIn
                        occupiedSeats
                        (nextGridUsing
                            Grid.adjacent
                            (\c -> c >= 4)
                        )
                        grid
                )
                result

        r2 =
            Result.map
                (\grid ->
                    Loop.untilNoChangeIn
                        occupiedSeats
                        (nextGridUsing
                            (Grid.lineOfSightUntil ((/=) Floor))
                            (\c -> c >= 5)
                        )
                        grid
                )
                result
    in
    ( Result.map String.fromInt r1
    , Result.map String.fromInt r2
    )


occupiedSeats : Grid Cell -> Int
occupiedSeats =
    Grid.foldl
        (\cell prev ->
            case cell of
                Occupied ->
                    1 + prev

                _ ->
                    prev
        )
        0


nextGridUsing : (( Int, Int ) -> Grid Cell -> List Cell) -> (Int -> Bool) -> Grid Cell -> Grid Cell
nextGridUsing getNeighbours shouldFree grid =
    Grid.indexedMap
        (\pos cell ->
            let
                occupied =
                    getNeighbours pos grid
                        |> List.map
                            (\seat ->
                                case seat of
                                    Occupied ->
                                        1

                                    Free ->
                                        0

                                    Floor ->
                                        0
                            )
                        |> List.sum
            in
            case cell of
                Free ->
                    if occupied == 0 then
                        Occupied

                    else
                        cell

                Occupied ->
                    if shouldFree occupied then
                        Free

                    else
                        cell

                Floor ->
                    Floor
        )
        grid


charToCell : Char -> Result String Cell
charToCell c =
    case c of
        'L' ->
            Ok Free

        '#' ->
            Ok Occupied

        '.' ->
            Ok Floor

        _ ->
            Err ("Unrecongnized char: " ++ String.fromChar c)
