module Year2015.Day02 exposing (..)

import Performance exposing (Performance)
import List.Extra as List
import Result.Extra as Result
import Util.Vec3 as Vec3


solution =
    { solve = solve
    , title = "I Was Told There Would Be No Math"
    , subtitle = "The elves needs wrapping paper and ribbons for wrapping boxes."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        boxes =
            input
                |> String.lines
                |> List.map
                    (String.split "x"
                        >> Vec3.fromList
                        >> Maybe.andThen
                            (Vec3.map String.toInt
                                >> Vec3.combine
                            )
                        >> Result.fromMaybe "Not a vector"
                    )
                |> Result.combine

        r1 =
            boxes
                |> Result.map
                    (List.map
                        (\box ->
                            let
                                v3 =
                                    Vec3.multiplySides box

                                smallestSide =
                                    Vec3.lowest v3
                            in
                            Vec3.sum v3 * 2 + smallestSide
                        )
                        >> List.sum
                        >> (\n -> String.fromInt n ++ " square feet of wrapping paper")
                    )

        r2 =
            boxes
                |> Result.map
                    (List.map
                        (\box ->
                            let
                                (low, mid, hi ) =
                                    Vec3.lowToHigh box
                            in
                            low * 2 + mid * 2 + Vec3.product box
                        )
                        >> List.sum
                        >> (\n -> String.fromInt n ++ " feet of ribbon")
                    )
    in
    ( r1
    , r2
    )
