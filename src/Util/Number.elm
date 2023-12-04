module Util.Number exposing (..)

import Array exposing (Array)


findTwoThatSumsTo : Int -> List Int -> Maybe ( Int, Int )
findTwoThatSumsTo n list =
    let
        arr =
            List.sort list
                |> Array.fromList
    in
    findTwoThatSumsToHelper
        0
        (Array.length arr - 1)
        n
        arr


findTwoThatSumsToHelper : Int -> Int -> Int -> Array Int -> Maybe ( Int, Int )
findTwoThatSumsToHelper start end n sortedArr =
    if start == end then
        Nothing

    else
        case ( Array.get start sortedArr, Array.get end sortedArr ) of
            ( Just n1, Just n2 ) ->
                case compare (n1 + n2) n of
                    LT ->
                        findTwoThatSumsToHelper (start + 1) end n sortedArr

                    EQ ->
                        Just ( n1, n2 )

                    GT ->
                        findTwoThatSumsToHelper start (end - 1) n sortedArr

            _ ->
                Nothing


lowHighIn : List Int -> Maybe ( Int, Int )
lowHighIn list =
    case list of
        _ :: _ :: _ ->
            List.foldl
                (\n ( l, h ) ->
                    ( min n l
                    , max n h
                    )
                )
                ( intMax, intMin )
                list
                |> Just

        _ ->
            Nothing


intMax : Int
intMax =
    2 ^ 53 - 1


intMin : Int
intMin =
    -(2 ^ 53)


