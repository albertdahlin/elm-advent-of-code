module Year2021.Day04 exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Set exposing (Set)
import Util.Parser


solution =
    { solve = solve
    , title = "Giant Squid"
    , subtitle = "Play bingo with a giant squid."
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Board =
    { a : Row, b : Row, c : Row, d : Row, e : Row }


type alias Row =
    { a : Int, b : Int, c : Int, d : Int, e : Int }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        result =
            Util.Parser.run parser input

        r1 =
            result
                |> Result.map
                    (\( numbers, boards ) ->
                        case playUntilNextWin numbers Set.empty boards of
                            Just turn ->
                                winnerScores turn
                                    |> List.map String.fromInt
                                    |> String.join ", "

                            Nothing ->
                                "No Winner"
                    )

        r2 =
            result
                |> Result.map
                    (\( numbers, boards ) ->
                        case findLastWinner Nothing numbers Set.empty boards of
                            Just turn ->
                                winnerScores turn
                                    |> List.map String.fromInt
                                    |> String.join ", "

                            Nothing ->
                                "No Winner"
                    )
    in
    ( r1
    , r2
    )



-- PLAY BINGO


type alias Turn =
    { winners : List Board
    , remainingPlayers : List Board
    , playedNumbers : Set Int
    , winningNumber : Int
    , remainingNumbers : List Int
    }


playUntilNextWin : List Int -> Set Int -> List Board -> Maybe Turn
playUntilNextWin numbersNotPlayed played boards =
    case numbersNotPlayed of
        [] ->
            Nothing

        x :: xs ->
            let
                playedNumbers =
                    Set.insert x played
            in
            case List.partition (isAnyComplete playedNumbers) boards of
                ( [], didNotWin ) ->
                    playUntilNextWin xs playedNumbers boards

                ( winners, didNotWin ) ->
                    { winners = winners
                    , remainingPlayers = didNotWin
                    , playedNumbers = playedNumbers
                    , winningNumber = x
                    , remainingNumbers = xs
                    }
                        |> Just


findLastWinner : Maybe Turn -> List Int -> Set Int -> List Board -> Maybe Turn
findLastWinner lastTurn numbers played boards =
    case boards of
        [] ->
            lastTurn

        _ ->
            case playUntilNextWin numbers played boards of
                Just turn ->
                    findLastWinner
                        (Just turn)
                        turn.remainingNumbers
                        turn.playedNumbers
                        turn.remainingPlayers

                Nothing ->
                    lastTurn



-- CALCULATE SCORES

{-| Technically there can be more than one winner in a turn
, this is why we return a list of scores.
-}
winnerScores : Turn -> List Int
winnerScores turn =
    List.map
        (\winner ->
            calculateScore
                winner
                turn.playedNumbers
                turn.winningNumber
        )
        turn.winners


{-| Calculate score for one board.
-}
calculateScore : Board -> Set Int -> Int -> Int
calculateScore board playedNumbers winningNumber =
    numbersByRow board
        |> List.concat
        |> List.filter (\n -> Set.member n playedNumbers |> not)
        |> List.sum
        |> (*) winningNumber



-- BOARDS


numbersByRow : Board -> List (List Int)
numbersByRow board =
    [ rowNumbers board.a
    , rowNumbers board.b
    , rowNumbers board.c
    , rowNumbers board.d
    , rowNumbers board.e
    ]


numbersByCol : Board -> List (List Int)
numbersByCol board =
    [ colNumbers .a board
    , colNumbers .b board
    , colNumbers .c board
    , colNumbers .d board
    , colNumbers .e board
    ]


rowNumbers : Row -> List Int
rowNumbers row =
    [ row.a, row.b, row.c, row.d, row.e ]


colNumbers : (Row -> Int) -> Board -> List Int
colNumbers getCol board =
    [ getCol board.a
    , getCol board.b
    , getCol board.c
    , getCol board.d
    , getCol board.e
    ]


numbersToCheck : Board -> List (List Int)
numbersToCheck board =
    [ numbersByRow board
    , numbersByCol board
    ]
        |> List.concat


isAnyComplete : Set Int -> Board -> Bool
isAnyComplete set board =
    numbersToCheck board
        |> List.any (List.all (\n -> Set.member n set))



-- INPUT PARSER


parser : Parser ( List Int, List Board )
parser =
    Parser.succeed Tuple.pair
        |= Parser.sequence
            { start = ""
            , separator = ","
            , end = "\n"
            , spaces = Parser.chompWhile (\c -> c == ' ')
            , item = Parser.int
            , trailing = Parser.Optional
            }
        |. Parser.spaces
        |= boardsParser


boardsParser : Parser (List Board)
boardsParser =
    Parser.loop []
        (\xs ->
            Parser.oneOf
                [ Parser.succeed (\x -> Parser.Loop (x :: xs))
                    |= boardParser
                    |. Parser.spaces
                , Parser.succeed (Parser.Done <| List.reverse xs)
                ]
        )


boardParser : Parser Board
boardParser =
    Parser.succeed Board
        |= rowParser
        |. Util.Parser.newLine
        |= rowParser
        |. Util.Parser.newLine
        |= rowParser
        |. Util.Parser.newLine
        |= rowParser
        |. Util.Parser.newLine
        |= rowParser


rowParser : Parser Row
rowParser =
    Parser.succeed Row
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= Parser.int
