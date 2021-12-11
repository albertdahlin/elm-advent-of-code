module Util.Mat2d exposing (..)

import Array exposing (Array)


type Mat2d a
    = Mat2d Int Int (Array a)


init : ( Int, Int ) -> a -> Mat2d a
init ( w, h ) a =
    Mat2d (max 1 w) (max 1 h) (Array.repeat (max 1 w * max 1 h) a)


get : ( Int, Int ) -> Mat2d a -> Maybe a
get ( x, y ) (Mat2d w h arr) =
    if x < 0 || y < 0 || x > (w - 1) || y > (h - 1) then
        Nothing

    else
        Array.get (y * w + x) arr


getBounded : ( Int, Int ) -> Mat2d a -> a
getBounded ( x, y ) (Mat2d w h arr) =
    case Array.get (clamp 0 (h - 1) y * w + clamp 0 (w - 1) x) arr of
        Just a ->
            a

        Nothing ->
            Debug.todo "out of bounds"


getWrap : ( Int, Int ) -> Mat2d a -> a
getWrap ( x, y ) (Mat2d w h arr) =
    case Array.get (modBy h (y * w) + modBy w x) arr of
        Just a ->
            a

        Nothing ->
            Debug.todo "out of bounds"


set : ( Int, Int ) -> a -> Mat2d a -> Mat2d a
set ( x, y ) v (Mat2d w h arr) =
    Mat2d w h (Array.set (y * w + x) v arr)


map : (( Int, Int ) -> a -> b) -> Mat2d a -> Mat2d b
map fn (Mat2d w h arr) =
    Mat2d w
        h
        (Array.indexedMap
            (\idx a -> fn ( modBy w idx, idx // w ) a)
            arr
        )


foldl : (( Int, Int ) -> a -> b -> b) -> b -> Mat2d a -> b
foldl fn st (Mat2d w h arr) =
    Array.foldl
        (\a ( i, s ) ->
            ( i + 1
            , fn ( modBy w i, i // w ) a s
            )
        )
        ( 0, st )
        arr
        |> Tuple.second


rows : Mat2d a -> List (List a)
rows (Mat2d w h arr) =
    let
        cols =
            List.range 0 (w - 1)
    in
    List.range 0 (h - 1)
        |> List.map (\y -> List.filterMap (\x -> Array.get (y * w + x) arr) cols)


columns : Mat2d a -> List (List a)
columns (Mat2d w h arr) =
    let
        rs =
            List.range 0 (h - 1)
    in
    List.range 0 (w - 1)
        |> List.map (\x -> List.filterMap (\y -> Array.get (y * w + x) arr) rs)


around : ( Int, Int ) -> Mat2d a -> List a
around ( x, y ) (Mat2d w h arr) =
    (if y == 0 then
        if x <= 0 then
            [ Array.get (y * w + x + 1) arr
            , Array.get ((y + 1) * w + x) arr
            , Array.get ((y + 1) * w + x + 1) arr
            ]

        else if x == (w - 1) then
            [ Array.get (y * w + x - 1) arr
            , Array.get ((y + 1) * w + x - 1) arr
            , Array.get ((y + 1) * w + x) arr
            ]

        else
            [ Array.get (y * w + x - 1) arr
            , Array.get (y * w + x + 1) arr
            , Array.get ((y + 1) * w + x - 1) arr
            , Array.get ((y + 1) * w + x) arr
            , Array.get ((y + 1) * w + x + 1) arr
            ]

     else if y == (h - 1) then
        if x <= 0 then
            [ Array.get ((y - 1) * w + x) arr
            , Array.get ((y - 1) * w + x + 1) arr
            , Array.get (y * w + x + 1) arr
            ]

        else if x == (w - 1) then
            [ Array.get ((y - 1) * w + x - 1) arr
            , Array.get ((y - 1) * w + x) arr
            , Array.get (y * w + x - 1) arr
            ]

        else
            [ Array.get ((y - 1) * w + x - 1) arr
            , Array.get ((y - 1) * w + x) arr
            , Array.get ((y - 1) * w + x + 1) arr
            , Array.get (y * w + x - 1) arr
            , Array.get (y * w + x + 1) arr
            ]

     else if x <= 0 then
        [ Array.get ((y - 1) * w + x) arr
        , Array.get ((y - 1) * w + x + 1) arr
        , Array.get (y * w + x + 1) arr
        , Array.get ((y + 1) * w + x) arr
        , Array.get ((y + 1) * w + x + 1) arr
        ]

     else if x == (w - 1) then
        [ Array.get ((y - 1) * w + x) arr
        , Array.get ((y - 1) * w + x - 1) arr
        , Array.get (y * w + x - 1) arr
        , Array.get ((y + 1) * w + x) arr
        , Array.get ((y + 1) * w + x - 1) arr
        ]

     else
        [ Array.get ((y - 1) * w + x - 1) arr
        , Array.get ((y - 1) * w + x) arr
        , Array.get ((y - 1) * w + x + 1) arr
        , Array.get (y * w + x - 1) arr
        , Array.get (y * w + x + 1) arr
        , Array.get ((y + 1) * w + x - 1) arr
        , Array.get ((y + 1) * w + x) arr
        , Array.get ((y + 1) * w + x + 1) arr
        ]
    )
        |> List.filterMap identity


forLinesAndChars : String -> (( Int, Int ) -> Char -> s -> s) -> s -> s
forLinesAndChars str fn state =
    String.lines str
        |> List.indexedMap
            (\y row ->
                String.toList row
                    |> List.indexedMap (\x char -> ( ( x, y ), char ))
            )
        |> List.concat
        |> List.foldl
            (\( k, v ) ->
                fn k v
            )
            state
