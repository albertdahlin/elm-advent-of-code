module Util.Digits exposing (..)


type Digits
    = Digits Int (List Int)


fromListWithBase : Int -> List Int -> Digits
fromListWithBase base list =
    Digits base (List.reverse list)


toList : Digits -> List Int
toList (Digits _ list) =
    List.reverse list


add : Int -> Digits -> Digits
add n (Digits base list) =
    addHelp n base list
        |> Digits base


addHelp : Int -> Int -> List Int -> List Int
addHelp n base list =
    case list of
        [] ->
            [ n ]

        digit :: rest ->
            let
                sum =
                    digit + n

                carry =
                    sum // base
            in
            if carry > 0 then
                remainderBy base sum
                    :: addHelp carry base rest

            else
                sum :: rest

