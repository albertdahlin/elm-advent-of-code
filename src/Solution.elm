module Solution exposing (..)

import Year2015.Day01
import Year2020.Day01
import Dict exposing (Dict)


type alias Solution =
    String -> ( Result String String, Result String String )


forYear : Int -> Dict Int Solution
forYear y =
    case y of
        2015 ->
            year2015

        2020 ->
            year2020

        _ ->
            Dict.empty


year2015 : Dict Int Solution
year2015 =
    [ ( 1, Year2015.Day01.solve )
    ]
        |> Dict.fromList


year2020 : Dict Int Solution
year2020 =
    [ ( 1, Year2020.Day01.solve )
    ]
        |> Dict.fromList
