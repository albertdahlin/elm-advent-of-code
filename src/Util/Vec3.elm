module Util.Vec3 exposing (..)


type alias Vec3 a =
    ( a, a, a )


product : Vec3 number -> number
product ( x, y, z ) =
    x * y * z


multiplySides : Vec3 number -> Vec3 number
multiplySides ( x, y, z ) =
    ( x * y, y * z, x * z )


sum : Vec3 number -> number
sum ( x, y, z ) =
    x + y + z


lowest : Vec3 number -> number
lowest ( x, y, z ) =
    min x y
        |> min z


lowToHigh : Vec3 comparable -> Vec3 comparable
lowToHigh ( x, y, z ) =
    case List.sort [ x, y, z ] of
        [ l1, l2, l3 ] ->
            ( l1, l2, l3 )

        _ ->
            ( x, y, z )


fromList : List a -> Maybe (Vec3 a)
fromList list =
    case list of
        [ x, y, z ] ->
            Just ( x, y, z )

        _ ->
            Nothing


map : (a -> b) -> Vec3 a -> Vec3 b
map fn ( x, y, z ) =
    ( fn x, fn y, fn z )


combine : Vec3 (Maybe a) -> Maybe (Vec3 a)
combine ( mbX, mbY, mbZ ) =
    Maybe.map3
        (\x y z ->
            ( x, y, z )
        )
        mbX
        mbY
        mbZ
