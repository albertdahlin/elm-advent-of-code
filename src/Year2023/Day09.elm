module Year2023.Day09 exposing (..)

import List.Extra as List
import Performance
import Util.Function as Fn
import Util.List as List
import Util.String as String
import Util.Tuple as Tuple


solution =
    { solve = solve
    , title = "Mirage Maintenance"
    , subtitle = "Predict the next value using historic data from the OASIS report"
    , tests = []
    , performance = Performance.Acceptable
    }


sequenceOfDiffs : List Int -> List (List Int)
sequenceOfDiffs =
    List.buildUntil
        (List.all ((==) 0))
        (List.mapWindow2 (Fn.flip (-)))


extrapolateRightSide : List (List Int) -> Int
extrapolateRightSide =
    List.map (List.lastOr 0)
        >> List.foldr (+) 0


extrapolateLeftSide : List (List Int) -> Int
extrapolateLeftSide =
    List.map (List.firstOr 0)
        >> List.foldr (-) 0


solve : String -> ( Result String String, Result String String )
solve =
    String.lines
        >> List.map
            (String.parseInts
                -- List of numbers : List Int
                >> sequenceOfDiffs
                -- List of diffs : List (List Int)
                >> Tuple.from
                    extrapolateRightSide
                    extrapolateLeftSide
             -- Extrapolated number for part 1, part 2 : ( Int, Int )
            )
        -- Rows with extrapolated numbers : List ( Int, Int )
        >> Tuple.from
            (List.map Tuple.first)
            (List.map Tuple.second)
        -- Numbers for part 1 and 2 : ( List Int, List Int )
        >> Tuple.map
            (List.sum >> String.fromInt >> Ok)
