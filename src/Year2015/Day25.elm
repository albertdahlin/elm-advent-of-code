module Year2015.Day25 exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Util.Parser
import Util.Repeat as Repeat


solution =
    { solve = solve
    , title = "Let It Snow"
    , subtitle = "Crack the code to Santa's weather machine"
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        r1 =
            Util.Parser.run parser input
                |> Result.map
                    (\pos ->
                        Repeat.nTimes (findNth pos - 1) next start
                            |> String.fromInt
                    )
    in
    ( r1
    , r1
    )


findNth : ( Int, Int ) -> Int
findNth ( row, col ) =
    let
        colAtRow1 =
            col + (row - 1)

        nAtCol1 =
            colAtRow1 * (colAtRow1 + 1) // 2
    in
    nAtCol1 - (row - 1)


start : Int
start =
    20151125


next : Int -> Int
next n =
    modBy 33554393 (n * 252533)



-- PARSER


parser : Parser ( Int, Int )
parser =
    Parser.succeed Tuple.pair
        |. Parser.chompWhile (Char.isDigit >> not)
        |= Util.Parser.int
        |. Parser.chompWhile (Char.isDigit >> not)
        |= Util.Parser.int
