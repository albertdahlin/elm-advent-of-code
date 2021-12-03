module Year2021.Day03 exposing (..)

import Performance exposing (Performance)


solution =
    { solve = solve
    , title = "Binary Diagnostic"
    , subtitle = "Decode information from the submarines diagnostic report."
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Bits =
    List Bool


solve : String -> ( Result String String, Result String String )
solve input =
    let
        numbers =
            String.lines input
                |> List.map parseBits

        numbersCount =
            List.length numbers // 2

        gamma =
            numbers
                |> countOneBits
                |> toGamma numbersCount

        epsilon =
            List.map not gamma

        oxy =
            filterByFirstBit identity numbers

        co2 =
            filterByFirstBit (List.map not) numbers

        r1 =
            (toInt gamma * toInt epsilon)
                |> String.fromInt
                |> Ok

        r2 =
            (toInt oxy * toInt co2)
                |> String.fromInt
                |> Ok
    in
    ( r1
    , r2
    )


filterByFirstBit : (Bits -> Bits) -> List Bits -> Bits
filterByFirstBit applyCriteria numbers =
    case numbers of
        [] ->
            []

        [ n ] ->
            n

        _ ->
            let
                numbersCount =
                    (List.length numbers |> toFloat)
                        / 2
                        |> ceiling

                mostCommonBits =
                    numbers
                        |> countOneBits
                        |> toGamma numbersCount
                        |> applyCriteria

                firstBit =
                    List.head mostCommonBits
                        |> Maybe.withDefault True

                {- Filter out numbers matching the first bit and at the
                   same time throw away the first bit.
                -}
                matchingNumbers =
                    List.filterMap
                        (\bits ->
                            case bits of
                                [] ->
                                    Nothing

                                x :: xs ->
                                    if x == firstBit then
                                        Just xs

                                    else
                                        Nothing
                        )
                        numbers
            in
            firstBit :: filterByFirstBit applyCriteria matchingNumbers


toGamma : Int -> List Int -> Bits
toGamma count bits =
    List.map
        (\ones ->
            if ones >= count then
                True

            else
                False
        )
        bits


countOneBits : List Bits -> List Int
countOneBits numbers =
    List.foldl
        countOneBitsHelp
        []
        numbers


countOneBitsHelp : Bits -> List Int -> List Int
countOneBitsHelp bits counts =
    case bits of
        [] ->
            counts

        x :: xs ->
            let
                count =
                    List.head counts
                        |> Maybe.withDefault 0
            in
            if x then
                count + 1 :: countOneBitsHelp xs (List.drop 1 counts)

            else
                count :: countOneBitsHelp xs (List.drop 1 counts)


parseBits : String -> Bits
parseBits str =
    String.toList str
        |> List.map (\char -> char == '1')


toInt : Bits -> Int
toInt list =
    List.reverse list
        |> List.indexedMap
            (\idx bit ->
                if bit then
                    2 ^ idx

                else
                    0
            )
        |> List.sum
