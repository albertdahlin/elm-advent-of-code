module Year2015.Day24 exposing (..)

import Performance exposing (Performance)


solution =
    { solve = solve
    , title = "It Hangs in the Balance"
    , subtitle = "Balance the weight of all packages in Santa's sleigh."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        numbers =
            String.lines input
                |> List.filterMap String.toInt
                |> List.sort
                |> List.reverse

        sum =
            List.sum numbers

        r1 =
            findBest (sum // 3) numbers []
                |> List.product
                |> String.fromInt
                |> Ok

        r2 =
            findBest (sum // 4) numbers []
                |> List.product
                |> String.fromInt
                |> Ok
    in
    ( r1
    , r2
    )


findBest : Int -> List Int -> List Int -> List Int
findBest target list taken =
    let
        sum =
            List.sum taken
    in
    case list of
        [] ->
            if sum == target then
                taken

            else
                []

        x :: xs ->
            if (x + sum) > target then
                findBest target xs taken

            else if (x + sum) == target then
                bestOf
                    (x :: taken)
                    (findBest target xs taken)

            else
                bestOf
                    (findBest target xs taken)
                    (findBest target xs (x :: taken))


bestOf : List Int -> List Int -> List Int
bestOf a b =
    let
        lenA =
            List.length a

        lenB =
            List.length b
    in
    if lenA == 0 then
        b

    else if lenB == 0 then
        a

    else if lenA == lenB then
        let
            prodA =
                List.product a

            prodB =
                List.product b
        in
        if prodA < prodB then
            a

        else
            b

    else if lenA < lenB then
        a

    else
        b
