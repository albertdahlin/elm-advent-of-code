module Util.Vec2 exposing (..)


type alias Vec2 a =
    ( a, a )


add : Vec2 number -> Vec2 number -> Vec2 number
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


fromList : List a -> Maybe (Vec2 a)
fromList list =
    case list of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing


filterMap : (a -> Maybe b) -> Vec2 a -> Maybe (Vec2 b)
filterMap fn ( x, y ) =
    Maybe.map2 Tuple.pair (fn x) (fn y)


sum : Vec2 number -> number
sum ( x, y ) =
    x + y
