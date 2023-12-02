module Year2015.Day10 exposing (..)

import Array exposing (Array)
import Performance exposing (Performance)
import Util.Loop


solution =
    { solve = solve
    , title = "Elves Look, Elves Say"
    , subtitle = ""
    , tests = []
    , performance = Performance.Acceptable
    }


step : Array String -> Array String
step list =
    Array.foldl
        (\char ( prev, count, arr ) ->
            if prev == char then
                ( char
                , count + 1
                , arr
                )

            else
                ( char
                , 1
                , arr
                    |> Array.push (String.fromInt count)
                    |> Array.push prev
                )
        )
        ( Array.get 0 list
            |> Maybe.withDefault ""
        , 0
        , Array.empty
        )
        list
        |> (\( char, count, arr ) ->
                arr
                    |> Array.push (String.fromInt count)
                    |> Array.push char
           )


solve : String -> ( Result String String, Result String String )
solve input =
    let
        listAfter40 =
            String.split "" input
                |> Array.fromList
                |> Util.Loop.repeat 40 step

        p1 =
            Array.length listAfter40
                |> String.fromInt

        p2 =
            listAfter40
                |> Util.Loop.repeat 10 step
                |> Array.length
                |> String.fromInt
    in
    ( Ok p1
    , Ok p2
    )
