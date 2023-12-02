module Util.Regex exposing (..)

import Regex exposing (Regex)
import Result.Extra as Result


parseInt : String -> String -> Result String Int
parseInt rgx row =
    Regex.fromString rgx
        |> Result.fromMaybe "Regex syntax err"
        |> Result.andThen
            (\regex ->
                Regex.find regex row
                    |> List.head
                    |> Maybe.andThen
                        (.submatches
                            >> List.head
                            >> Maybe.andThen identity
                        )
                    |> Maybe.andThen String.toInt
                    |> Result.fromMaybe "Not a number"
            )


parsePairs : String -> String -> Result String (List ( String, String ))
parsePairs rgx row =
    Regex.fromString rgx
        |> Result.fromMaybe "Regex syntax err"
        |> Result.andThen
            (\regex ->
                Regex.find regex row
                    |> List.map
                        (\match ->
                            case match.submatches of
                                [ Just fst, Just snd ] ->
                                    Ok ( fst, snd )

                                _ ->
                                    Err "Not two matches"
                        )
                    |> Result.combine
            )
