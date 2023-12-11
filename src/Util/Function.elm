module Util.Function exposing (..)


flip : (a -> b -> c) -> (b -> a -> c)
flip fn a b =
    fn b a


branch isGood thenCase elseCase a =
    if isGood a then
        thenCase a

    else
        elseCase a
