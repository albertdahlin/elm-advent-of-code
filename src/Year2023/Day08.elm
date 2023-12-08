module Year2023.Day08 exposing (..)

import Arithmetic
import Dict exposing (Dict)
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Util.Parser


solution =
    { solve = solve
    , title = "Haunted Wasteland"
    , subtitle = "Navigate thorugh the Desert Island using \"maps\""
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Input =
    { instructions : List Instruction
    , maps : Dict String ( String, String )
    }


type Instruction
    = Left
    | Right


parseInput : String -> Result String Input
parseInput input =
    let
        alphaNum =
            Util.Parser.nonEmptyStringWhile Char.isAlphaNum

        parseInstructions =
            Util.Parser.listWhile
                (Parser.oneOf
                    [ Parser.succeed Left
                        |. Parser.symbol "L"
                    , Parser.succeed Right
                        |. Parser.symbol "R"
                    ]
                )

        parseNode =
            Parser.succeed
                (\key right left ->
                    ( key, ( right, left ) )
                )
                |= alphaNum
                |. Parser.spaces
                |. Parser.symbol "="
                |. Parser.spaces
                |. Parser.symbol "("
                |= alphaNum
                |. Parser.symbol ","
                |. Parser.spaces
                |= alphaNum
                |. Parser.symbol ")"
                |. Parser.spaces
    in
    Util.Parser.run
        (Parser.succeed Input
            |= parseInstructions
            |. Parser.spaces
            |= (Util.Parser.listWhile parseNode
                    |> Parser.map Dict.fromList
               )
        )
        input


leftOrRight : Instruction -> ( String, String ) -> String
leftOrRight instruction ( left, right ) =
    case instruction of
        Right ->
            right

        Left ->
            left


countSteps : Input -> String -> Int
countSteps input key =
    runUntilZ 1 input input.instructions key


runUntilZ : Int -> Input -> List Instruction -> String -> Int
runUntilZ count input instructions key =
    case instructions of
        [] ->
            runUntilZ count input input.instructions key

        inst :: rest ->
            case Dict.get key input.maps of
                Just tuple ->
                    let
                        newKey =
                            leftOrRight inst tuple
                    in
                    if String.endsWith "Z" newKey then
                        count

                    else
                        runUntilZ (count + 1) input rest newKey

                Nothing ->
                    -100



-- PART 1


solve1 : Input -> String
solve1 input =
    countSteps input "AAA"
        |> String.fromInt



-- PART 2


solve2 : Input -> String
solve2 input =
    Dict.keys input.maps
        |> List.filter (String.endsWith "A")
        |> List.concatMap (countSteps input >> Arithmetic.primeFactors)
        |> List.Extra.unique
        |> List.product
        |> String.fromInt


solve : String -> ( Result String String, Result String String )
solve input =
    let
        parsedInput =
            parseInput input
    in
    ( Result.map solve1 parsedInput
    , Result.map solve2 parsedInput
    )
