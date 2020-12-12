module Year2020.Day08 exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Result.Extra as Result
import Util.Parser


solution =
    { solve = solve
    , title = "Handheld Halting"
    , subtitle = "Help a kid fixing his handheld console."
    , tests = []
    , performance = Performance.Acceptable
    }


type Instr
    = Nop Int
    | Jmp Int
    | Acc Int


type alias Program =
    Array ( Instr, Bool )


solve : String -> ( Result String String, Result String String )
solve input =
    let
        program : Result String Program
        program =
            parse input
                |> Result.map
                    (List.map (\i -> ( i, False ))
                        >> Array.fromList
                    )

        r1 =
            program
                |> Result.map
                    (eval_AbortOnRepeated 0 0
                        >> format
                    )

        r2 =
            program
                |> Result.andThen
                    (eval_TrySwappingOne False 0 0
                        >> Result.fromMaybe "No solution"
                        >> Result.map format
                    )

        format i =
            "acc = " ++ String.fromInt i
    in
    ( r1
    , r2
    )


eval_AbortOnRepeated : Int -> Int -> Program -> Int
eval_AbortOnRepeated idx acc program =
    case Array.get idx program of
        Just ( instr, alreadyEvaluated ) ->
            if alreadyEvaluated then
                acc

            else
                let
                    nextProgram =
                        setAsEvaluated idx program
                in
                case instr of
                    Nop arg ->
                        eval_AbortOnRepeated (idx + 1) acc nextProgram

                    Jmp arg ->
                        eval_AbortOnRepeated (idx + arg) acc nextProgram

                    Acc arg ->
                        eval_AbortOnRepeated (idx + 1) (acc + arg) nextProgram

        Nothing ->
            acc


setAsEvaluated : Int -> Program -> Program
setAsEvaluated idx program =
    Array.update idx (\( i, _ ) -> ( i, True )) program


swapInstr : Int -> Program -> Program
swapInstr idx program =
    Array.update
        idx
        (\( i, _ ) ->
            case i of
                Nop arg ->
                    ( Jmp arg, False )

                Jmp arg ->
                    ( Nop arg, False )

                Acc arg ->
                    ( Acc arg, False )
        )
        program


eval_TrySwappingOne : Bool -> Int -> Int -> Program -> Maybe Int
eval_TrySwappingOne alreadySwapped idx acc program =
    case Array.get idx program of
        Just ( instr, alreadyEvaluated ) ->
            if alreadyEvaluated then
                Nothing

            else
                let
                    nextProgram =
                        setAsEvaluated idx program
                in
                case instr of
                    Nop arg ->
                        case eval_TrySwappingOne alreadySwapped (idx + 1) acc nextProgram of
                            Nothing ->
                                if alreadySwapped then
                                    Nothing

                                else
                                    eval_TrySwappingOne True idx acc (swapInstr idx program)

                            Just i ->
                                Just i

                    Jmp arg ->
                        case eval_TrySwappingOne alreadySwapped (idx + arg) acc nextProgram of
                            Nothing ->
                                if alreadySwapped then
                                    Nothing

                                else
                                    eval_TrySwappingOne True idx acc (swapInstr idx program)

                            Just i ->
                                Just i

                    Acc arg ->
                        eval_TrySwappingOne alreadySwapped (idx + 1) (acc + arg) nextProgram

        Nothing ->
            Just acc


parse : String -> Result String (List Instr)
parse str =
    String.lines str
        |> List.map
            (\instr ->
                case String.split " " instr of
                    [ fst, snd ] ->
                        case String.toInt snd of
                            Just i ->
                                case fst of
                                    "acc" ->
                                        Ok (Acc i)

                                    "jmp" ->
                                        Ok (Jmp i)

                                    "nop" ->
                                        Ok (Nop i)

                                    _ ->
                                        Err ("Unknown: " ++ instr)

                            Nothing ->
                                Err ("Arg not an int: " ++ instr)

                    _ ->
                        Err ("Row problem: " ++ instr)
            )
        |> Result.combine
