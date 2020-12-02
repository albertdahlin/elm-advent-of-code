module Year2015.Day01 exposing (..)

import Performance exposing (Performance)
import List.Extra as List
import Result.Extra as Result


solution =
    { solve = solve
    , title = "Not Quite Lisp"
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        vectors =
            String.toList input
                |> List.map
                    (\c ->
                        case c of
                            '(' ->
                                Ok 1

                            ')' ->
                                Ok -1

                            _ ->
                                Err ("Invalid char: " ++ String.fromChar c)
                    )
                |> Result.combine

        r1 =
            vectors
                |> Result.map (\i -> "Final floor: " ++ (List.sum i |> String.fromInt))

        r2 =
            vectors
                |> Result.andThen
                    (List.scanl
                        (+)
                        0
                        >> List.elemIndex -1
                        >> Maybe.map (\i -> "Enters basement at: " ++ String.fromInt i)
                        >> Result.fromMaybe "Never enters basement"
                    )
    in
    ( r1
    , r2
    )
