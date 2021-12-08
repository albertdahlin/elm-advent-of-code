module Year2021.Day08 exposing (..)

import Dict exposing (Dict)
import Performance exposing (Performance)
import Result.Extra as Result
import Set exposing (Set)


solution =
    { solve = solve
    , title = "Seven Segment Search"
    , subtitle = "Figure out how the wires of the malfunctioning seven-segment display are connected."
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Segments =
    { one : Set Char -- 2 segments must be 1
    , seven : Set Char -- 3 segments must be 7
    , four : Set Char -- 4 segments must be 4
    , twoThreeFive : List (Set Char) -- 2, 3, 5
    , zeroSixNine : List (Set Char) -- 0, 6, 9
    , eight : Set Char -- 7 segments must be 8
    }


solve : String -> ( Result String String, Result String String )
solve rawInput =
    let
        parsedRows : Result String (List ( Segments, List String ))
        parsedRows =
            String.lines rawInput
                |> List.map
                    (\line ->
                        case String.split " | " line of
                            [ input, out ] ->
                                String.words input
                                    |> List.map (String.toList >> Set.fromList)
                                    |> List.foldl groupBySize Dict.empty
                                    |> parseSegments
                                    |> Result.map
                                        (\i ->
                                            ( i
                                            , String.words out
                                            )
                                        )

                            _ ->
                                Err "err"
                    )
                |> Result.combine

        r1 =
            parsedRows
                |> Result.map
                    (\rows ->
                        rows
                            |> List.map
                                (\( _, output ) ->
                                    List.map String.length output
                                        |> List.filter (\l -> l == 2 || l == 4 || l == 3 || l == 7)
                                        |> List.length
                                )
                            |> List.sum
                            |> String.fromInt
                    )

        r2 =
            parsedRows
                |> Result.andThen
                    (List.map
                        (\( input, output ) ->
                            makeCorrectionMap input
                                |> Result.andThen
                                    (\map ->
                                        List.map (replaceCharsFrom map) output
                                            |> Result.combine
                                    )
                                |> Result.andThen
                                    (\segments ->
                                        List.map segmentsToInt segments
                                            |> Result.combine
                                    )
                                |> Result.map digitsToInt
                        )
                        >> Result.combine
                        >> Result.map (List.sum >> String.fromInt)
                    )
    in
    ( r1
    , r2
    )


makeCorrectionMap : Segments -> Result String (Dict Char Char)
makeCorrectionMap seg =
    Result.map4
        (\a d f g ->
            Result.map3
                (\b c e ->
                    [ ( a, 'a' )
                    , ( b, 'b' )
                    , ( c, 'c' )
                    , ( d, 'd' )
                    , ( e, 'e' )
                    , ( f, 'f' )
                    , ( g, 'g' )
                    ]
                        |> Dict.fromList
                )
                (findSegmentB seg a f g)
                (findSegmentC seg f)
                (findSegmentE seg a g)
        )
        (findSegmentA seg)
        (findSegmentD seg)
        (findSegmentF seg)
        (findSegmentG seg)
        |> Result.andThen identity


findSegmentA : Segments -> Result String Char
findSegmentA seg =
    Set.diff seg.seven seg.one
        |> extractSingleton


findSegmentB : Segments -> Char -> Char -> Char -> Result String Char
findSegmentB seg a f g =
    List.foldl Set.intersect seg.eight seg.zeroSixNine
        |> Set.remove a
        |> Set.remove f
        |> Set.remove g
        |> extractSingleton


findSegmentC : Segments -> Char -> Result String Char
findSegmentC seg f =
    Set.remove f seg.one
        |> extractSingleton


findSegmentD : Segments -> Result String Char
findSegmentD seg =
    List.foldl Set.intersect seg.four seg.twoThreeFive
        |> extractSingleton


findSegmentE : Segments -> Char -> Char -> Result String Char
findSegmentE seg a g =
    Set.diff seg.eight seg.four
        |> Set.remove a
        |> Set.remove g
        |> extractSingleton


findSegmentF : Segments -> Result String Char
findSegmentF seg =
    List.foldl Set.intersect seg.one seg.zeroSixNine
        |> extractSingleton


findSegmentG : Segments -> Result String Char
findSegmentG seg =
    seg.twoThreeFive
        |> List.map
            (\set ->
                Set.union seg.four seg.seven
                    |> Set.diff set
            )
        |> findSingletonSet
        |> Result.andThen extractSingleton



-- SET


findSingletonSet : List (Set a) -> Result String (Set a)
findSingletonSet list =
    List.filter (\set -> Set.size set == 1) list
        |> List.head
        |> Result.fromMaybe "Not singleton set found"


extractSingleton : Set a -> Result String a
extractSingleton set =
    if Set.size set == 1 then
        case Set.toList set of
            [ a ] ->
                Ok a

            _ ->
                Err "WTF"

    else
        Err "Not a singleton set"


{-| Convert a list of digits to an int
-}
digitsToInt : List Int -> Int
digitsToInt digits =
    digits
        |> List.reverse
        |> List.indexedMap
            (\idx d ->
                d * (10 ^ idx)
            )
        |> List.sum


replaceCharsFrom : Dict Char Char -> String -> Result String String
replaceCharsFrom map str =
    String.toList str
        |> List.map (\c -> Dict.get c map |> Result.fromMaybe "Char not in map")
        |> Result.combine
        |> Result.map (List.sort >> String.fromList)


{-| abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg | cf
-}
segmentsToInt : String -> Result String Int
segmentsToInt w =
    case w of
        "abcefg" ->
            Ok 0

        "cf" ->
            Ok 1

        "acdeg" ->
            Ok 2

        "acdfg" ->
            Ok 3

        "bcdf" ->
            Ok 4

        "abdfg" ->
            Ok 5

        "abdefg" ->
            Ok 6

        "acf" ->
            Ok 7

        "abcdefg" ->
            Ok 8

        "abcdfg" ->
            Ok 9

        _ ->
            Err ("Not a valid segment: " ++ w)


groupBySize : Set a -> Dict Int (List (Set a)) -> Dict Int (List (Set a))
groupBySize set dict =
    Dict.update
        (Set.size set)
        (\mbList ->
            case mbList of
                Just list ->
                    Just (set :: list)

                Nothing ->
                    Just [ set ]
        )
        dict



-- PARSE


parseSegments : Dict Int (List (Set Char)) -> Result String Segments
parseSegments dict =
    Ok
        (\one seven four twoThreeFive zeroSixNine eight ->
            { one = one
            , seven = seven
            , four = four
            , twoThreeFive = twoThreeFive
            , zeroSixNine = zeroSixNine
            , eight = eight
            }
        )
        |> Result.andMap
            (Dict.get 2 dict
                |> Maybe.andThen List.head
                |> Result.fromMaybe "No digit with 2 segments"
            )
        |> Result.andMap
            (Dict.get 3 dict
                |> Maybe.andThen List.head
                |> Result.fromMaybe "No digit with 3 segments"
            )
        |> Result.andMap
            (Dict.get 4 dict
                |> Maybe.andThen List.head
                |> Result.fromMaybe "No digit with 4 segments"
            )
        |> Result.andMap
            (Dict.get 5 dict
                |> Result.fromMaybe "No digit with 5 segments"
            )
        |> Result.andMap
            (Dict.get 6 dict
                |> Result.fromMaybe "No digit with 6 segments"
            )
        |> Result.andMap
            (Dict.get 7 dict
                |> Maybe.andThen List.head
                |> Result.fromMaybe "No digit with 7 segments"
            )
