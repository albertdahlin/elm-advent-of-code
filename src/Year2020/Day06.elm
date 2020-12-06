module Year2020.Day06 exposing (..)

import List.Extra as List
import Performance exposing (Performance)
import Set exposing (Set)


solution =
    { solve = solve
    , title = "Custom Customs"
    , subtitle = "Help plane passengers to fill out customs forms."
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Group =
    List Person


type alias Person =
    Set Char


solve : String -> ( Result String String, Result String String )
solve input =
    let
        groups : List Group
        groups =
            String.split "\n\n" input
                |> List.map
                    (String.split "\n"
                        >> List.map (String.toList >> Set.fromList)
                    )

        r1 =
            groups
                |> List.map
                    (foldSetsWith Set.union
                        >> Set.size
                    )
                |> List.sum
                |> String.fromInt
                |> Ok

        r2 =
            groups
                |> List.map
                    (foldSetsWith Set.intersect
                        >> Set.size
                    )
                |> List.sum
                |> String.fromInt
                |> Ok
    in
    ( r1
    , r2
    )


foldSetsWith : (Set a -> Set a -> Set a) -> List (Set a) -> Set a
foldSetsWith fn =
    List.foldl1 fn
        >> Maybe.withDefault Set.empty
