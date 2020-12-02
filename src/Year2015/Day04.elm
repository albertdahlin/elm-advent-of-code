module Year2015.Day04 exposing (..)

import Performance exposing (Performance)
import List.Extra as List
import Result.Extra as Result
import MD5


solution =
    { solve = solve
    , title = "The Ideal Stocking Stuffer (slow)"
    , subtitle = "Help Santa to mine AdventCoins using MD5 hashes."
    , tests = []
    , performance = Performance.Terrible
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        r1 =
            mineUntil (String.startsWith "00000") 0 input
                |> String.fromInt
                |> Ok

        r2 =
            mineUntil (String.startsWith "000000") 0 input
                |> String.fromInt
                |> Ok
    in
    ( r1
    , r2
    )


mineUntil : (String -> Bool) -> Int -> String -> Int
mineUntil isGood nuance str =
    let
        hash =
            MD5.hex (str ++ String.fromInt nuance)
    in
    if isGood hash then
        nuance
    else
        mineUntil isGood (nuance + 1) str

