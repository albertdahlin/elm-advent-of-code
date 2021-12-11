module Util.Grid2 exposing (..)

import Dict exposing (Dict)


type alias Grid a =
    Dict ( Int, Int ) a


{-| Size ( height, width )
-}
size : Grid a -> ( Int, Int )
size grid =
    Dict.foldl
        (\( r, c ) _ ( h, w ) ->
            ( min r h
            , min c w
            )
        )
        ( 10000, 10000 )
        grid


rows : Grid a -> List (List a)
rows grid =
    Dict.foldl
        (\( r, c ) v ->
            Dict.update
                r
                (\mb ->
                    case mb of
                        Just row ->
                            Dict.insert c v row
                                |> Just

                        Nothing ->
                            Dict.singleton c v
                                |> Just
                )
        )
        Dict.empty
        grid
        |> Dict.values
        |> List.map Dict.values


columns : Grid a -> List (List a)
columns grid =
    Dict.foldl
        (\( r, c ) v ->
            Dict.update
                c
                (\mb ->
                    case mb of
                        Just row ->
                            Dict.insert r v row
                                |> Just

                        Nothing ->
                            Dict.singleton r v
                                |> Just
                )
        )
        Dict.empty
        grid
        |> Dict.values
        |> List.map Dict.values


updateAt : ( Int, Int ) -> (a -> a) -> Grid a -> Grid a
updateAt point fn grid =
    Dict.update point (Maybe.map fn) grid


adjacent : ( Int, Int ) -> List ( Int, Int )
adjacent ( r, c ) =
    [ ( r - 1, c - 1 )
    , ( r - 1, c )
    , ( r - 1, c + 1 )
    , ( r, c - 1 )
    , ( r, c + 1 )
    , ( r + 1, c - 1 )
    , ( r + 1, c )
    , ( r + 1, c + 1 )
    ]


neighbors : ( Int, Int ) -> Grid a -> List ( ( Int, Int ), a )
neighbors pos grid =
    adjacent pos
        |> List.filterMap
            (\p -> Dict.get p grid |> Maybe.map (Tuple.pair p))


foldAround : ( Int, Int ) -> (( Int, Int ) -> a -> Grid a -> Grid a) -> Grid a -> Grid a
foldAround pos fn grid =
    adjacent pos
        |> List.foldl
            (\point grid2 ->
                case Dict.get point grid2 of
                    Just val ->
                        fn point val grid2

                    Nothing ->
                        grid2
            )
            grid


all : (( Int, Int ) -> a -> Bool) -> Grid a -> Bool
all isGood grid =
    Dict.foldl
        (\k v prev ->
            prev && isGood k v
        )
        True
        grid


any : (( Int, Int ) -> a -> Bool) -> Grid a -> Bool
any isGood grid =
    Dict.foldl
        (\k v prev ->
            prev || isGood k v
        )
        False
        grid


forLinesAndChars : String -> (( Int, Int ) -> Char -> s -> s) -> s -> s
forLinesAndChars str fn state =
    String.lines str
        |> List.indexedMap
            (\ri row ->
                String.toList row
                    |> List.indexedMap (\ci char -> ( ( ri, ci ), char ))
            )
        |> List.concat
        |> List.foldl
            (\( k, v ) ->
                fn k v
            )
            state
