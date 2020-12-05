module Year2015.Day06 exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Result.Extra as Result
import Util.Parser
import Util.Vec2 as Vec2


type alias Vec2 =
    Vec2.Vec2 Int


type alias Grid =
    Array Int


type Instr
    = TurnOn Vec2 Vec2
    | Toggle Vec2 Vec2
    | TurnOff Vec2 Vec2


solution =
    { solve = solve
    , title = "Probably a Fire Hazard (slow)"
    , subtitle = "Configure lights on a 1000x1000 grid."
    , tests = []
    , performance = Performance.Terrible
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        instructions =
            Parser.run parseManyInstructions input
                |> Result.mapError Util.Parser.firstErrorMsg

        r1 =
            instructions
                |> Result.map
                    (List.foldl
                        (\instr grid2 ->
                            case instr of
                                TurnOn from to ->
                                    forEachPoint
                                        (\x y -> Array.set (x + y * 1000) 1)
                                        from
                                        to
                                        grid2

                                TurnOff from to ->
                                    forEachPoint
                                        (\x y -> Array.set (x + y * 1000) 0)
                                        from
                                        to
                                        grid2

                                Toggle from to ->
                                    forEachPoint
                                        (\x y ->
                                            Array.update (x + y * 1000)
                                                (\n ->
                                                    if n == 0 then
                                                        1

                                                    else
                                                        0
                                                )
                                        )
                                        from
                                        to
                                        grid2
                        )
                        (Array.repeat (1000 * 1000) 0)
                        >> Array.foldl (+) 0
                        >> (\sz -> String.fromInt sz)
                    )

        r2 =
            instructions
                |> Result.map
                    (List.foldl
                        (\instr grid2 ->
                            case instr of
                                TurnOn from to ->
                                    forEachPoint
                                        (\x y -> Array.update (x + y * 1000) ((+) 1))
                                        from
                                        to
                                        grid2

                                TurnOff from to ->
                                    forEachPoint
                                        (\x y -> Array.update (x + y * 1000) ((+) -1 >> max 0))
                                        from
                                        to
                                        grid2

                                Toggle from to ->
                                    forEachPoint
                                        (\x y -> Array.update (x + y * 1000) ((+) 2))
                                        from
                                        to
                                        grid2
                        )
                        (Array.repeat (1000 * 1000) 0)
                        >> Array.foldl (+) 0
                        >> (\sz -> String.fromInt sz)
                    )
    in
    ( r1
    , r2
    )


forEachPoint : (Int -> Int -> a -> a) -> Vec2 -> Vec2 -> a -> a
forEachPoint fn ( x1, y1 ) ( x2, y2 ) grid =
    let
        allY =
            List.range y1 y2
    in
    List.range x1 x2
        |> List.foldl
            (\x grid2 ->
                List.foldl
                    (\y ->
                        fn x y
                    )
                    grid2
                    allY
            )
            grid


parseManyInstructions : Parser (List Instr)
parseManyInstructions =
    Parser.loop [] instructionsHelp


instructionsHelp : List Instr -> Parser (Parser.Step (List Instr) (List Instr))
instructionsHelp revInstr =
    Parser.oneOf
        [ Parser.succeed (\instr -> Parser.Loop (instr :: revInstr))
            |= parseInstruction
            |. Parser.spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revInstr))
        ]


parseInstruction : Parser Instr
parseInstruction =
    Parser.oneOf
        [ Parser.keyword "turn"
            |. Parser.spaces
            |> Parser.andThen
                (\_ ->
                    Parser.oneOf
                        [ Parser.succeed TurnOn
                            |. Parser.keyword "on"
                            |. Parser.spaces
                            |= parseVec2
                            |. Parser.spaces
                            |. Parser.keyword "through"
                            |. Parser.spaces
                            |= parseVec2
                        , Parser.succeed TurnOff
                            |. Parser.keyword "off"
                            |. Parser.spaces
                            |= parseVec2
                            |. Parser.spaces
                            |. Parser.keyword "through"
                            |. Parser.spaces
                            |= parseVec2
                        ]
                )
        , Parser.succeed Toggle
            |. Parser.keyword "toggle"
            |. Parser.spaces
            |= parseVec2
            |. Parser.spaces
            |. Parser.keyword "through"
            |. Parser.spaces
            |= parseVec2
        ]


parseVec2 : Parser Vec2
parseVec2 =
    Parser.succeed Tuple.pair
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol ","
        |. Parser.spaces
        |= Parser.int
