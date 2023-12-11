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


getOr default key dict =
    Dict.get key dict
        |> Maybe.withDefault default


fillFrom dict keys =
    case keys of
        [] ->
            []

        key :: rest ->
            case Dict.get key dict of
                Just val ->
                    ( key, val ) :: fillFrom dict rest

                Nothing ->
                    fillFrom dict rest


getMany : List comparable -> Dict comparable a -> List a
getMany keys dict =
    List.filterMap (\k -> Dict.get k dict) keys


getManyFrom : Dict comparable a -> List comparable -> List a
getManyFrom dict keys =
    List.filterMap (\k -> Dict.get k dict) keys


from2 : (a -> a -> b) -> comparable -> comparable -> Dict comparable a -> Maybe b
from2 fn k1 k2 dict =
    Maybe.map2 fn
        (Dict.get k1 dict)
        (Dict.get k2 dict)


from3 :
    (a -> a -> a -> b)
    -> comparable
    -> comparable
    -> comparable
    -> Dict comparable a
    -> Maybe b
from3 fn k1 k2 k3 dict =
    Maybe.map3 fn
        (Dict.get k1 dict)
        (Dict.get k2 dict)
        (Dict.get k3 dict)


allOf : List ( comparable, a -> Bool ) -> Dict comparable a -> Bool
allOf list dict =
    fromKeys list dict
        |> List.all identity


fromKeys : List ( comparable, a -> b ) -> Dict comparable a -> List b
fromKeys list dict =
    case list of
        [] ->
            []

        ( k, fn ) :: rest ->
            case Dict.get k dict of
                Just a ->
                    fn a :: fromKeys rest dict

                Nothing ->
                    fromKeys rest dict


tupleFrom : comparable -> comparable -> a -> Dict comparable a -> ( a, a )
tupleFrom k1 k2 default dict =
    ( Dict.get k1 dict |> Maybe.withDefault default
    , Dict.get k2 dict |> Maybe.withDefault default
    )


tupleFrom3 : comparable -> comparable -> comparable -> a -> Dict comparable a -> ( a, a, a )
tupleFrom3 k1 k2 k3 default dict =
    ( Dict.get k1 dict |> Maybe.withDefault default
    , Dict.get k2 dict |> Maybe.withDefault default
    , Dict.get k3 dict |> Maybe.withDefault default
    )


aggregateList : (a -> a -> a) -> List ( comparable, a ) -> Dict comparable a
aggregateList fn list =
    List.foldl
        (\( key, val ) ->
            Dict.update
                key
                (\mb ->
                    case mb of
                        Just v ->
                            fn val v
                                |> Just

                        Nothing ->
                            Just val
                )
        )
        Dict.empty
        list


aggregate :
    (a -> a -> a)
    -> comparable
    -> a
    -> Dict comparable a
    -> Dict comparable a
aggregate fn key val =
    Dict.update
        key
        (\mb ->
            case mb of
                Just v ->
                    fn val v
                        |> Just

                Nothing ->
                    Just val
        )


aggregateTuple :
    (a -> a -> a)
    -> ( comparable, a )
    -> Dict comparable a
    -> Dict comparable a
aggregateTuple fn ( key, val ) =
    Dict.update
        key
        (\mb ->
            case mb of
                Just v ->
                    fn val v
                        |> Just

                Nothing ->
                    Just val
        )


aggregateUnion :
    (a -> a -> a)
    -> Dict comparable a
    -> Dict comparable a
    -> Dict comparable a
aggregateUnion fn dict1 dict2 =
    Dict.foldl
        (\key val ->
            aggregate fn key val
        )
        dict1
        dict2


groupBy :
    (a -> comparable)
    -> List a
    -> Dict comparable (List a)
groupBy fn =
    List.foldl
        (\a ->
            Dict.update
                (fn a)
                (\mb ->
                    case mb of
                        Just list ->
                            Just (a :: list)

                        Nothing ->
                            Just [ a ]
                )
        )
        Dict.empty
