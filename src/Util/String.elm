module Util.String exposing (..)

import Regex exposing (Regex)


parseInts : String -> List Int
parseInts row =
    Regex.find
        (Regex.fromString "(-?\\d+)"
            |> Maybe.withDefault Regex.never
        )
        row
        |> List.concatMap
            (\match ->
                List.filterMap (Maybe.andThen String.toInt) match.submatches
            )
