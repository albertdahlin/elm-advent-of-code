module Util.List exposing (..)

import List.Extra


firstOr : a -> List a -> a
firstOr default list =
    case list of
        x :: _ ->
            x

        _ ->
            default


lastOr : a -> List a -> a
lastOr default list =
    case List.reverse list of
        x :: _ ->
            x

        _ ->
            default


mapWindow2 : (a -> a -> b) -> List a -> List b
mapWindow2 fn list =
    mapWindow2Help [] fn list
        |> List.reverse


mapWindow2Help acc fn list =
    case list of
        x :: y :: rest ->
            mapWindow2Help (fn x y :: acc) fn (y :: rest)

        _ ->
            acc


mapWindow3 : (a -> a -> a -> b) -> List a -> List b
mapWindow3 fn list =
    case list of
        x :: y :: z :: rest ->
            fn x y z :: mapWindow3 fn (y :: rest)

        _ ->
            []


when2 : (a -> a -> b) -> List a -> Maybe b
when2 fn list =
    case list of
        [ a, b ] ->
            Just (fn a b)

        _ ->
            Nothing


when2or : b -> (a -> a -> b) -> List a -> b
when2or default fn list =
    case list of
        [ a, b ] ->
            fn a b

        _ ->
            default


buildUntil : (a -> Bool) -> (a -> a) -> a -> List a
buildUntil isDone next a =
    if isDone a then
        [ a ]

    else
        a :: buildUntil isDone next (next a)


indexIn : List a -> a -> Maybe Int
indexIn list element =
    List.Extra.elemIndex element list
