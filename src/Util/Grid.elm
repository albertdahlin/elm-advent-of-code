module Util.Grid exposing (..)

import Array exposing (Array)
import List.Extra as List
import Util.Vec2 as Vec2


type Grid a
    = Grid
        { width : Int
        , height : Int
        , rows : Array (Array a)
        }


fromRows : Array (Array a) -> Maybe (Grid a)
fromRows rows =
    let
        mbWidth =
            case Array.map Array.length rows |> Array.toList of
                x :: xs ->
                    if List.all ((==) x) xs then
                        Just x

                    else
                        Nothing

                [] ->
                    Just 0
    in
    case mbWidth of
        Just w ->
            { width = w
            , height = Array.length rows
            , rows = rows
            }
                |> Grid
                |> Just

        Nothing ->
            Nothing


get : ( Int, Int ) -> Grid a -> Maybe a
get ( row, col ) (Grid grid) =
    Array.get row grid.rows
        |> Maybe.andThen (Array.get col)


width : Grid a -> Int
width (Grid grid) =
    grid.width


height : Grid a -> Int
height (Grid grid) =
    grid.height


adjacent : ( Int, Int ) -> Grid a -> List a
adjacent rowCol grid =
    [ lookUntil (always True) rowCol ( -1, -1 ) grid
    , lookUntil (always True) rowCol ( -1, 0 ) grid
    , lookUntil (always True) rowCol ( -1, 1 ) grid
    , lookUntil (always True) rowCol ( 0, -1 ) grid
    , lookUntil (always True) rowCol ( 0, 1 ) grid
    , lookUntil (always True) rowCol ( 1, -1 ) grid
    , lookUntil (always True) rowCol ( 1, 0 ) grid
    , lookUntil (always True) rowCol ( 1, 1 ) grid
    ]
        |> List.filterMap identity


lineOfSightUntil : (a -> Bool) -> ( Int, Int ) -> Grid a -> List a
lineOfSightUntil shouldStop rowCol grid =
    [ lookUntil shouldStop rowCol ( -1, -1 ) grid
    , lookUntil shouldStop rowCol ( -1, 0 ) grid
    , lookUntil shouldStop rowCol ( -1, 1 ) grid
    , lookUntil shouldStop rowCol ( 0, -1 ) grid
    , lookUntil shouldStop rowCol ( 0, 1 ) grid
    , lookUntil shouldStop rowCol ( 1, -1 ) grid
    , lookUntil shouldStop rowCol ( 1, 0 ) grid
    , lookUntil shouldStop rowCol ( 1, 1 ) grid
    ]
        |> List.filterMap identity


lookUntil : (a -> Bool) -> ( Int, Int ) -> ( Int, Int ) -> Grid a -> Maybe a
lookUntil shouldStop pos dir grid =
    let
        nextPos =
            Vec2.add pos dir
    in
    case get nextPos grid of
        Just cell ->
            case shouldStop cell of
                True ->
                    Just cell

                False ->
                    lookUntil shouldStop nextPos dir grid

        Nothing ->
            Nothing


indexedMap : (( Int, Int ) -> a -> b) -> Grid a -> Grid b
indexedMap fn (Grid grid) =
    { rows =
        grid.rows
            |> Array.indexedMap
                (\rowNo ->
                    Array.indexedMap
                        (\colNo cell ->
                            fn ( rowNo, colNo ) cell
                        )
                )
    , width = grid.width
    , height = grid.height
    }
        |> Grid


foldl : (a -> b -> b) -> b -> Grid a -> b
foldl fn init (Grid grid) =
    Array.foldl
        (\row state ->
            Array.foldl
                (\cell state2 ->
                    fn cell state2
                )
                state
                row
        )
        init
        grid.rows
