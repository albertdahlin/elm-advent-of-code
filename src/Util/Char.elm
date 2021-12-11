module Util.Char exposing (..)


toInt : Char -> Maybe Int
toInt =
    String.fromChar >> String.toInt
