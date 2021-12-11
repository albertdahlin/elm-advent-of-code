module Util.Repeat exposing (..)


nTimes : Int -> (a -> a) -> a -> a
nTimes n fn arg =
    if n <= 1 then
        fn arg

    else
        nTimes (n - 1) fn (fn arg)


whileJust : (a -> Maybe a) -> a -> a
whileJust fn arg =
    case fn arg of
        Just next ->
            whileJust fn next

        Nothing ->
            arg
