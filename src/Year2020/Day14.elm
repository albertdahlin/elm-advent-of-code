module Year2020.Day14 exposing (..)

import Dict exposing (Dict)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Result.Extra as Result
import UInt64 exposing (UInt64)
import Util.Parser


solution =
    { solve = solve
    , title = "Docking Data"
    , subtitle = "Execute the ferry docking program using bitmasks."
    , tests = []
    , performance = Performance.Acceptable
    }


type Instr
    = Mask Masks
    | Mem Int UInt64


type alias Masks =
    { ones : UInt64
    , zeros : UInt64
    , xs : List Int
    }


emptyMask : Masks
emptyMask =
    { ones = UInt64.zero
    , zeros = UInt64.zero
    , xs = []
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        result : Result String (List Instr)
        result =
            Parser.run parser input
                |> Result.mapError Util.Parser.firstErrorMsg

        r1 =
            result
                |> Result.map
                    (List.foldl
                        (\instr ( memory, masks ) ->
                            case instr of
                                Mask newMasks ->
                                    ( memory
                                    , newMasks
                                    )

                                Mem addr value ->
                                    ( Dict.insert
                                        addr
                                        (UInt64.and masks.zeros value
                                            |> UInt64.or masks.ones
                                        )
                                        memory
                                    , masks
                                    )
                        )
                        ( Dict.empty
                        , emptyMask
                        )
                        >> Tuple.first
                        >> Dict.foldl (\_ -> UInt64.add) (UInt64.fromInt 0)
                        >> UInt64.toString
                    )

        r2 =
            result
                |> Result.map
                    (List.foldl
                        (\instr ( memory, masks ) ->
                            case instr of
                                Mask newMasks ->
                                    ( memory
                                    , newMasks
                                    )

                                Mem idx value ->
                                    let
                                        addresses =
                                            permuteFloatingBits
                                                (idx
                                                    |> UInt64.fromInt
                                                    |> UInt64.or masks.ones
                                                )
                                                masks.xs
                                    in
                                    ( List.foldl
                                        (\addr -> Dict.insert (UInt64.toString addr) value)
                                        memory
                                        addresses
                                    , masks
                                    )
                        )
                        ( Dict.empty
                        , emptyMask
                        )
                        >> Tuple.first
                        >> Dict.foldl (\_ -> UInt64.add) (UInt64.fromInt 0)
                        >> UInt64.toString
                    )
    in
    ( r1
    , r2
    )


permuteFloatingBits : UInt64 -> List Int -> List UInt64
permuteFloatingBits base indicesOfX =
    case indicesOfX of
        [] ->
            []

        idx :: rest ->
            let
                a =
                    UInt64.setBit idx 0 base

                b =
                    UInt64.setBit idx 1 base
            in
            a :: b :: permuteFloatingBits a rest ++ permuteFloatingBits b rest


parser : Parser (List Instr)
parser =
    Parser.loop []
        (\xs ->
            Parser.oneOf
                [ Parser.succeed (\x -> Parser.Loop (x :: xs))
                    |= parseInstr
                    |. Parser.spaces
                , Parser.succeed (Parser.Done (List.reverse xs))
                ]
        )


parseInstr : Parser Instr
parseInstr =
    Parser.oneOf
        [ parseMask
        , parseMem
        ]


parseMask : Parser Instr
parseMask =
    Parser.succeed
        (\str ->
            let
                bits =
                    String.toList str
                        |> List.reverse

                ones =
                    List.elemIndices '1' bits
                        |> List.foldl
                            (\idx -> UInt64.setBit idx 1)
                            UInt64.zero

                zeros =
                    List.elemIndices '0' bits
                        |> List.foldl
                            (\idx -> UInt64.setBit idx 0)
                            (UInt64.complement UInt64.zero)

                xs =
                    List.elemIndices 'X' bits
            in
            Mask
                { zeros = zeros
                , ones = ones
                , xs = xs
                }
        )
        |. Parser.keyword "mask"
        |. Parser.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= Parser.getChompedString
            (Parser.chompIf isBitMask
                |. Parser.chompWhile isBitMask
            )


isBitMask : Char -> Bool
isBitMask c =
    c == 'X' || c == '0' || c == '1'


parseMem : Parser Instr
parseMem =
    Parser.succeed (\idx val -> Mem idx (UInt64.fromInt val))
        |. Parser.keyword "mem"
        |. Parser.symbol "["
        |= Parser.int
        |. Parser.symbol "]"
        |. Parser.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= Parser.int
