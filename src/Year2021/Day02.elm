module Year2021.Day02 exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Result.Extra as Result
import Util.Parser
import Util.Vec2 as Vec2 exposing (Vec2)
import Util.Vec3 as Vec3 exposing (Vec3)


solution =
    { solve = solve
    , title = "Dive!"
    , subtitle = "Figure out how to pilot the submarine."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        instr =
            Parser.run (Util.Parser.parseRowsUsing dirParser) input
                |> Result.mapError Util.Parser.firstErrorMsg

        r1 =
            instr
                |> Result.map
                    (List.foldl move1 ( 0, 0 )
                        >> Vec2.product
                        >> String.fromInt
                    )

        r2 =
            instr
                |> Result.map
                    (List.foldl move2 ( 0, 0, 0 )
                        >> (\( x, y, z ) -> x * y)
                        >> String.fromInt
                    )
    in
    ( r1
    , r2
    )


type Dir
    = Up Int
    | Down Int
    | Forward Int


dirParser : Parser Dir
dirParser =
    Parser.oneOf
        [ Parser.keyword "up"
            |> Parser.map (always Up)
        , Parser.keyword "down"
            |> Parser.map (always Down)
        , Parser.keyword "forward"
            |> Parser.map (always Forward)
        ]
        |. Parser.spaces
        |= Parser.int


move1 : Dir -> Vec2 Int -> Vec2 Int
move1 dir ( hor, depth ) =
    case dir of
        Up l ->
            ( hor
            , depth - l
            )

        Down l ->
            ( hor
            , depth + l
            )

        Forward l ->
            ( hor + l
            , depth
            )


move2 : Dir -> Vec3 Int -> Vec3 Int
move2 dir ( hor, depth, aim ) =
    case dir of
        Up l ->
            ( hor
            , depth
            , aim - l
            )

        Down l ->
            ( hor
            , depth
            , aim + l
            )

        Forward l ->
            ( hor + l
            , depth + aim * l
            , aim
            )
