module Solution exposing (..)

import Year2015.Day01
import Year2015.Day02
import Year2015.Day03
import Year2015.Day04
import Year2015.Day05
import Year2020.Day01
import Year2020.Day02
import Dict exposing (Dict)


type alias Solution =
    { solve : String -> ( Result String String, Result String String )
    , title : String
    , tests : List ( String, String )
    }


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
    [ ( 1, Year2015.Day01.solution )
    , ( 2, Year2015.Day02.solution )
    , ( 3, Year2015.Day03.solution )
    , ( 4, Year2015.Day04.solution )
    , ( 5, Year2015.Day05.solution )
    ]
        |> Dict.fromList


year2020 : Dict Int Solution
year2020 =
    [ ( 1, Year2020.Day01.solution )
    , ( 2, Year2020.Day02.solution )
    ]
        |> Dict.fromList
