module Util.Char exposing (..)


toInt : Char -> Maybe Int
toInt =
    String.fromChar >> String.toInt


toAlphaIndex : Char -> Int
toAlphaIndex char =
    Char.toCode char - 65


fromAlphaIndex : Int -> Char
fromAlphaIndex code =
    Char.fromCode (code + 65)
