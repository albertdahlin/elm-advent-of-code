module Solution exposing (..)

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


type alias Solution =
    { solve : String -> ( Result String String, Result String String )
    , title : String
    , subtitle : String
    , tests : List ( String, String )
    , performance : Performance
    }


for : Int -> Int -> Maybe Solution
for year day =
    case year of
        2015 ->
            case day of
                1 ->
                    Just Year2015.Day01.solution

                2 ->
                    Just Year2015.Day02.solution

                3 ->
                    Just Year2015.Day03.solution

                4 ->
                    Just Year2015.Day04.solution

                5 ->
                    Just Year2015.Day05.solution

                6 ->
                    Just Year2015.Day06.solution

                7 ->
                    Just Year2015.Day07.solution

                8 ->
                    Just Year2015.Day08.solution

                9 ->
                    Just Year2015.Day09.solution

                24 ->
                    Just Year2015.Day24.solution

                25 ->
                    Just Year2015.Day25.solution

                _ ->
                    Nothing

        2016 ->
            case day of
                1 ->
                    Just Year2016.Day01.solution

                _ ->
                    Nothing

        2019 ->
            case day of
                1 ->
                    Just Year2019.Day01.solution

                _ ->
                    Nothing

        2020 ->
            case day of
                1 ->
                    Just Year2020.Day01.solution

                2 ->
                    Just Year2020.Day02.solution

                3 ->
                    Just Year2020.Day03.solution

                4 ->
                    Just Year2020.Day04.solution

                5 ->
                    Just Year2020.Day05.solution

                6 ->
                    Just Year2020.Day06.solution

                7 ->
                    Just Year2020.Day07.solution

                8 ->
                    Just Year2020.Day08.solution

                9 ->
                    Just Year2020.Day09.solution

                10 ->
                    Just Year2020.Day10.solution

                11 ->
                    Just Year2020.Day11.solution

                12 ->
                    Just Year2020.Day12.solution

                13 ->
                    Just Year2020.Day13.solution

                14 ->
                    Just Year2020.Day14.solution

                15 ->
                    Just Year2020.Day15.solution

                _ ->
                    Nothing

        2021 ->
            case day of
                1 ->
                    Just Year2021.Day01.solution

                2 ->
                    Just Year2021.Day02.solution

                3 ->
                    Just Year2021.Day03.solution

                4 ->
                    Just Year2021.Day04.solution

                5 ->
                    Just Year2021.Day05.solution

                6 ->
                    Just Year2021.Day06.solution

                7 ->
                    Just Year2021.Day07.solution

                8 ->
                    Just Year2021.Day08.solution

                9 ->
                    Just Year2021.Day09.solution

                10 ->
                    Just Year2021.Day10.solution

                11 ->
                    Just Year2021.Day11.solution

                _ ->
                    Nothing

        _ ->
            Nothing
