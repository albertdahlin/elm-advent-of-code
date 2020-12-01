module Util.Vec2 exposing (..)


type alias Vec2 a =
    ( a, a )


add : Vec2 number -> Vec2 number -> Vec2 number
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )
