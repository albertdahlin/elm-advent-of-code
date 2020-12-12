module Year2020.Day09 exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import List.Extra as List
import Performance exposing (Performance)
import Result.Extra as Result
import Util.Vec2 as Vec2
import Util.Number


solution =
    { solve = solve
    , title = "Encoding Error"
    , subtitle = "Decode the XMAS cypher from your plane entertainment system."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        allNumbers : Result String (List Int)
        allNumbers =
            String.lines input
                |> List.map (String.toInt >> Result.fromMaybe "Input is not numbers")
                |> Result.combine

        r1 =
            allNumbers
                |> Result.map Array.fromList
                |> Result.andThen (findFirstNonSum 25)

        r2 =
            Result.map2 findConsecutiveThatSumsTo r1 allNumbers
                |> Result.andThen identity
                |> Result.andThen (\list -> Util.Number.lowHighIn list |> Result.fromMaybe "No solution")
                |> Result.map Vec2.sum
    in
    ( Result.map String.fromInt r1
    , Result.map String.fromInt r2
    )


findFirstNonSum : Int -> Array Int -> Result String Int
findFirstNonSum preambleLength arr =
    let
        preamble =
            Array.sliceUntil preambleLength arr

        atNum =
            Array.get preambleLength arr
    in
    case atNum of
        Just n ->
            if Array.length preamble == preambleLength then
                case Util.Number.findTwoThatSumsTo n (Array.toList arr) of
                    Just ( x, y ) ->
                        findFirstNonSum preambleLength (Array.sliceFrom 1 arr)

                    Nothing ->
                        Ok n

            else
                Err "Preamble to short"

        Nothing ->
            Err "Index out of bounds"


findConsecutiveThatSumsTo : Int -> List Int -> Result String (List Int)
findConsecutiveThatSumsTo n list =
    findConsecutiveThatSumsTo_ n [] list


findConsecutiveThatSumsTo_ : Int -> List Int -> List Int -> Result String (List Int)
findConsecutiveThatSumsTo_ n toSum rest =
    case compare (List.sum toSum) n of
        LT ->
            case rest of
                [] ->
                    Err "No Sum"

                x :: xs ->
                    findConsecutiveThatSumsTo_ n (toSum ++ [ x ]) xs

        EQ ->
            Ok toSum

        GT ->
            case toSum of
                [] ->
                    Err "No Sum"

                x :: xs ->
                    findConsecutiveThatSumsTo_ n xs rest
