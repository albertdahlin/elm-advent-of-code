module Solution exposing (..)

import Dict exposing (Dict)
import Performance exposing (Performance)
import Year2015.Day01
import Year2015.Day02
import Year2015.Day03
import Year2015.Day04
import Year2015.Day05
import Year2015.Day06
import Year2015.Day07
import Year2015.Day08
import Year2015.Day09
import Year2015.Day24
import Year2015.Day25
import Year2016.Day01
import Year2019.Day01
import Year2020.Day01
import Year2020.Day02
import Year2020.Day03
import Year2020.Day04
import Year2020.Day05
import Year2020.Day06
import Year2020.Day07
import Year2020.Day08
import Year2020.Day09
import Year2020.Day10
import Year2020.Day11
import Year2020.Day12
import Year2020.Day13
import Year2020.Day14
import Year2020.Day15
import Year2021.Day01
import Year2021.Day02
import Year2021.Day03
import Year2021.Day04
import Year2021.Day05
import Year2021.Day06
import Year2021.Day07
import Year2021.Day08
import Year2021.Day09
import Year2021.Day10
import Year2021.Day11
import Year2021.Day12
import Year2022.Day01
import Year2022.Day02
import Year2022.Day07
import Year2023.Day01
import Year2023.Day02


type alias Solution =
    { solve : String -> ( Result String String, Result String String )
    , title : String
    , subtitle : String
    , tests : List ( String, String )
    , performance : Performance
    }


solutions : Dict ( Int, Int ) Solution
solutions =
    [ ( ( 2015, 1 ), Year2015.Day01.solution )
    , ( ( 2015, 2 ), Year2015.Day02.solution )
    , ( ( 2015, 3 ), Year2015.Day03.solution )
    , ( ( 2015, 4 ), Year2015.Day04.solution )
    , ( ( 2015, 5 ), Year2015.Day05.solution )
    , ( ( 2015, 6 ), Year2015.Day06.solution )
    , ( ( 2015, 7 ), Year2015.Day07.solution )
    , ( ( 2015, 8 ), Year2015.Day08.solution )
    , ( ( 2015, 9 ), Year2015.Day09.solution )
    , ( ( 2015, 24 ), Year2015.Day24.solution )
    , ( ( 2015, 25 ), Year2015.Day25.solution )

    -- 2016
    , ( ( 2016, 1 ), Year2016.Day01.solution )

    -- 2019
    , ( ( 2019, 1 ), Year2019.Day01.solution )

    -- 2020
    , ( ( 2020, 1 ), Year2020.Day01.solution )
    , ( ( 2020, 2 ), Year2020.Day02.solution )
    , ( ( 2020, 3 ), Year2020.Day03.solution )
    , ( ( 2020, 4 ), Year2020.Day04.solution )
    , ( ( 2020, 5 ), Year2020.Day05.solution )
    , ( ( 2020, 6 ), Year2020.Day06.solution )
    , ( ( 2020, 7 ), Year2020.Day07.solution )
    , ( ( 2020, 8 ), Year2020.Day08.solution )
    , ( ( 2020, 9 ), Year2020.Day09.solution )
    , ( ( 2020, 10 ), Year2020.Day10.solution )
    , ( ( 2020, 11 ), Year2020.Day11.solution )
    , ( ( 2020, 12 ), Year2020.Day12.solution )
    , ( ( 2020, 13 ), Year2020.Day13.solution )
    , ( ( 2020, 14 ), Year2020.Day14.solution )
    , ( ( 2020, 15 ), Year2020.Day15.solution )

    -- 2021
    , ( ( 2021, 1 ), Year2021.Day01.solution )
    , ( ( 2021, 2 ), Year2021.Day02.solution )
    , ( ( 2021, 3 ), Year2021.Day03.solution )
    , ( ( 2021, 4 ), Year2021.Day04.solution )
    , ( ( 2021, 5 ), Year2021.Day05.solution )
    , ( ( 2021, 6 ), Year2021.Day06.solution )
    , ( ( 2021, 7 ), Year2021.Day07.solution )
    , ( ( 2021, 8 ), Year2021.Day08.solution )
    , ( ( 2021, 9 ), Year2021.Day09.solution )
    , ( ( 2021, 10 ), Year2021.Day10.solution )
    , ( ( 2021, 11 ), Year2021.Day11.solution )
    , ( ( 2021, 12 ), Year2021.Day12.solution )

    -- 2022
    , ( ( 2022, 1 ), Year2022.Day01.solution )
    , ( ( 2022, 2 ), Year2022.Day02.solution )
    , ( ( 2022, 7 ), Year2022.Day07.solution )

    -- 2023
    , ( ( 2023, 1 ), Year2023.Day01.solution )
    , ( ( 2023, 2 ), Year2023.Day02.solution )
    ]
        |> Dict.fromList


for : Int -> Int -> Maybe Solution
for year day =
    Dict.get ( year, day ) solutions
