module Year2015.Day01 exposing (..)

import Result.Extra as Result
import List.Extra as List


solve : String -> ( Result String String, Result String String )
solve input =
    let
        r1 =
            String.toList input
                |> List.map
                    (\c ->
                        case c of
                            '(' -> Ok 1
                            ')' -> Ok -1
                            _ -> Err ("WTF: " ++ String.fromChar c)
                    )
                |> Result.combine
                |> Result.map (\i -> "Final floor: " ++ (List.sum i |> String.fromInt))

        r2 =
            String.toList input
                |> List.map
                    (\c ->
                        case c of
                            '(' -> Ok 1
                            ')' -> Ok -1
                            _ -> Err ("WTF: " ++ String.fromChar c)
                    )
                |> Result.combine
                |> Result.map
                    (List.scanl
                        (+)
                        0
                        >> List.elemIndex -1
                        >> Maybe.map (\i -> "Enters basement at: " ++ String.fromInt i)
                        >> Maybe.withDefault "Never enters basement"
                    )

    in
    ( r1
    , r2
    )
