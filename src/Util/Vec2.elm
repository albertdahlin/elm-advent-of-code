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


product : Vec2 number -> number
product ( x, y ) =
    x * y


sort : Vec2 number -> Vec2 number
sort ( x, y ) =
    if x < y then
        ( x, y )

    else
        ( y, x )


sub : Vec2 number -> Vec2 number -> Vec2 number
sub ( x1, y1 ) ( x2, y2 ) =
    ( x2 - x1
    , y2 - y1
    )


normalize : Vec2 Float -> Vec2 Float
normalize (( x, y ) as v) =
    let
        len =
            length v
    in
    ( x / len
    , y / len
    )


scaleLongestTo1 : Vec2 Float -> Vec2 Float
scaleLongestTo1 ( x, y ) =
    let
        m =
            max (abs x) (abs y)
    in
    ( x / m
    , y / m
    )


length : Vec2 Float -> Float
length ( x, y ) =
    sqrt (x * x + y * y)


manhattanDistance : Vec2 number -> number
manhattanDistance ( x, y ) =
    abs x + abs y


scale : number -> Vec2 number -> Vec2 number
scale s ( x, y ) =
    ( x * s
    , y * s
    )


toFloat : Vec2 Int -> Vec2 Float
toFloat =
    map Basics.toFloat


round : Vec2 Float -> Vec2 Int
round =
    map Basics.round


map : (a -> b) -> Vec2 a -> Vec2 b
map fn ( x, y ) =
    ( fn x
    , fn y
    )


fromAngle : Float -> Vec2 Float
fromAngle rad =
    ( cos rad
    , sin rad
    )


angleOf : Vec2 Float -> Float
angleOf ( x, y ) =
    atan2 y x
