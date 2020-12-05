module Year2020.Day05 exposing (..)

import Performance exposing (Performance)
import Result.Extra as Result
import Set exposing (Set)


solution =
    { solve = solve
    , title = "Binary Boarding"
    , subtitle = "Find your seat on the airplane."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        boardingPasses =
            String.lines input
                |> List.map parseBoardingPass
                |> Result.combine
                |> Result.andThen
                    (List.map
                        (\( row, col ) ->
                            let
                                ( rl, rh ) =
                                    List.foldl divideAndConquer ( 0, 127 ) row

                                ( cl, ch ) =
                                    List.foldl divideAndConquer ( 0, 7 ) col
                            in
                            if rl == rh then
                                if cl == ch then
                                    Ok ( rl, cl )

                                else
                                    Err "Column low and high are not equal"

                            else
                                Err "Row low and high are not equal"
                        )
                        >> Result.combine
                    )

        r1 =
            boardingPasses
                |> Result.map
                    (List.map toSeatId
                        >> List.maximum
                        >> Maybe.withDefault 0
                        >> (\id -> String.fromInt id ++ " is the highest seat ID")
                    )

        r2 =
            boardingPasses
                |> Result.map
                    (List.foldl Set.remove allSeats
                        >> Set.toList
                        >> List.map toSeatId
                        >> findFirstNonConsecutiveFrom 0
                        >> Maybe.map
                            (\id -> String.fromInt id ++ " is your seat ID")
                        >> Maybe.withDefault "No seat found"
                    )
    in
    ( r1
    , r2
    )


allSeats : Set ( Int, Int )
allSeats =
    List.range 0 127
        |> List.concatMap
            (\row ->
                List.range 0 7
                    |> List.map (\col -> ( row, col ))
            )
        |> Set.fromList


findFirstNonConsecutiveFrom : Int -> List Int -> Maybe Int
findFirstNonConsecutiveFrom prev list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x == prev then
                findFirstNonConsecutiveFrom (x + 1) xs

            else
                Just x


type Part
    = High
    | Low


toSeatId : ( Int, Int ) -> Int
toSeatId ( row, col ) =
    8 * row + col


divideAndConquer : Part -> ( Int, Int ) -> ( Int, Int )
divideAndConquer part ( low, high ) =
    let
        middle =
            (high - low) // 2
    in
    case part of
        High ->
            ( high - middle
            , high
            )

        Low ->
            ( low
            , low + middle
            )


parseBoardingPass : String -> Result String ( List Part, List Part )
parseBoardingPass str =
    case String.toList str of
        [ r0, r1, r2, r3, r4, r5, r6, c0, c1, c2 ] ->
            Result.map2
                (\row col ->
                    ( row
                    , col
                    )
                )
                (List.map parseRow [ r0, r1, r2, r3, r4, r5, r6 ]
                    |> Result.combine
                )
                (List.map parseCol [ c0, c1, c2 ]
                    |> Result.combine
                )

        _ ->
            Err ("Incorrect length: " ++ str)


parseRow : Char -> Result String Part
parseRow c =
    case c of
        'B' ->
            Ok High

        'F' ->
            Ok Low

        _ ->
            Err "Not a valid row"


parseCol : Char -> Result String Part
parseCol c =
    case c of
        'R' ->
            Ok High

        'L' ->
            Ok Low

        _ ->
            Err "Not a valid col"
