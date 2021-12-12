module Util.Dict exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


addToSet : comparable1 -> comparable2 -> Dict comparable1 (Set comparable2) -> Dict comparable1 (Set comparable2)
addToSet k v dict =
    Dict.update
        k
        (\mb ->
            case mb of
                Just set ->
                    Set.insert v set
                        |> Just

                Nothing ->
                    Set.singleton v
                        |> Just
        )
        dict


addToList : comparable -> a -> Dict comparable (List a) -> Dict comparable (List a)
addToList k v dict =
    Dict.update
        k
        (\mb ->
            case mb of
                Just list ->
                    Just (v :: list)

                Nothing ->
                    Just [ v ]
        )
        dict


addToSum : comparable -> number -> Dict comparable number -> Dict comparable number
addToSum k v dict =
    Dict.update
        k
        (\mb ->
            case mb of
                Just n ->
                    Just (v + n)

                Nothing ->
                    Just v
        )
        dict


groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy toKey list =
    List.foldl
        (\a ->
            addToList (toKey a) a
        )
        Dict.empty
        list
