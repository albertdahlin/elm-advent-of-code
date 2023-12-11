module Util.Tuple exposing (..)


from : (a -> x) -> (a -> y) -> a -> ( x, y )
from fn1 fn2 a =
    ( fn1 a
    , fn2 a
    )


curry : (a -> b -> c) -> ( a, b ) -> c
curry fn ( a, b ) =
    fn a b


uncurry : (( a, b ) -> c) -> a -> b -> c
uncurry fn a b =
    fn ( a, b )


flip : ( a, b ) -> ( b, a )
flip ( a, b ) =
    ( b, a )


sub : ( number, number ) -> ( number, number ) -> ( number, number )
sub ( a1, b1 ) ( a2, b2 ) =
    ( a1 - a2
    , b1 - b2
    )


binop : (a -> a -> a) -> ( a, a ) -> ( a, a ) -> ( a, a )
binop fn ( a1, b1 ) ( a2, b2 ) =
    ( fn a1 a2
    , fn b1 b2
    )


add : ( number, number ) -> ( number, number ) -> ( number, number )
add ( a1, b1 ) ( a2, b2 ) =
    ( a1 + a2
    , b1 + b2
    )


scaleBy : number -> ( number, number ) -> ( number, number )
scaleBy s ( a1, b1 ) =
    ( a1 * s
    , b1 * s
    )


product : ( number, number ) -> number
product ( fst, snd ) =
    fst * snd


sum : ( number, number ) -> number
sum ( fst, snd ) =
    fst + snd


concat : ( appendable, appendable ) -> appendable
concat ( a, b ) =
    a ++ b


combine : List ( a, b ) -> ( List a, List b )
combine list =
    ( List.map Tuple.first list
    , List.map Tuple.second list
    )


map : (a -> b) -> ( a, a ) -> ( b, b )
map fn ( x, y ) =
    ( fn x
    , fn y
    )


map3 : (a -> b) -> (c -> d) -> (e -> f) -> ( a, c, e ) -> ( b, d, f )
map3 fn1 fn2 fn3 ( a, c, e ) =
    ( fn1 a
    , fn2 c
    , fn3 e
    )
