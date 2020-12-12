module Year2020.Day12 exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Result.Extra as Result
import Util.Parser
import Util.Vec2 as Vec2 exposing (Vec2)


solution =
    { solve = solve
    , title = "Rain Risk"
    , subtitle = "Navigate the ferry through the storm."
    , tests = []
    , performance = Performance.Acceptable
    }


type Absolute
    = North
    | South
    | East
    | West


type Relative
    = Left
    | Right
    | Forward


type Dir
    = Relative Relative
    | Absolute Absolute


solve : String -> ( Result String String, Result String String )
solve input =
    let
        result : Result String (List ( Dir, Int ))
        result =
            Parser.run parse input
                |> Result.mapError Util.Parser.firstErrorMsg

        r1 =
            result
                |> Result.map solveFirst

        r2 =
            result
                |> Result.map solveSecond
    in
    ( Result.map String.fromInt r1
    , Result.map String.fromInt r2
    )


solveFirst : List ( Dir, Int ) -> Int
solveFirst =
    List.foldl
        (\( dir, len ) ( bearing, pos ) ->
            case dir of
                Absolute a ->
                    ( bearing
                    , fromPolar
                        ( toFloat len
                        , toAngle a
                        )
                        |> Vec2.round
                        |> Vec2.add pos
                    )

                Relative r ->
                    case r of
                        Left ->
                            ( bearing + (degrees <| toFloat len)
                            , pos
                            )

                        Right ->
                            ( bearing - (degrees <| toFloat len)
                            , pos
                            )

                        Forward ->
                            ( bearing
                            , fromPolar
                                ( toFloat len
                                , bearing
                                )
                                |> Vec2.round
                                |> Vec2.add pos
                            )
        )
        ( toAngle East
        , ( 0, 0 )
        )
        >> Tuple.second
        >> Vec2.manhattanDistance


solveSecond : List ( Dir, Int ) -> Int
solveSecond =
    List.foldl
        (\( dir, len ) ( waypoint, pos ) ->
            case dir of
                Absolute a ->
                    ( fromPolar ( toFloat len, toAngle a )
                        |> Vec2.round
                        |> Vec2.add waypoint
                    , pos
                    )

                Relative r ->
                    case r of
                        Left ->
                            ( Vec2.toFloat waypoint
                                |> toPolar
                                |> Vec2.add ( 0, degrees (toFloat len) )
                                |> fromPolar
                                |> Vec2.round
                            , pos
                            )

                        Right ->
                            ( Vec2.toFloat waypoint
                                |> toPolar
                                |> Vec2.add ( 0, -1 * degrees (toFloat len) )
                                |> fromPolar
                                |> Vec2.round
                            , pos
                            )

                        Forward ->
                            ( waypoint
                            , Vec2.scale len waypoint
                                |> Vec2.add pos
                            )
        )
        ( ( 10, 1 )
        , ( 0, 0 )
        )
        >> Tuple.second
        >> Vec2.manhattanDistance


toAngle : Absolute -> Float
toAngle dir =
    case dir of
        North ->
            pi / 2

        South ->
            pi / -2

        West ->
            pi

        East ->
            0


parse : Parser (List ( Dir, Int ))
parse =
    Parser.loop []
        (\xs ->
            Parser.oneOf
                [ Parser.succeed (\x -> Parser.Loop (x :: xs))
                    |= parseInstruction
                    |. Parser.spaces
                , Parser.succeed (Parser.Done (List.reverse xs))
                ]
        )


parseInstruction : Parser ( Dir, Int )
parseInstruction =
    Parser.succeed (\dir len -> ( dir, len ))
        |= parseDirection
        |= Parser.int


parseDirection : Parser Dir
parseDirection =
    Parser.getChompedString (Parser.chompIf (always True))
        |> Parser.andThen
            (\str ->
                case str of
                    "N" ->
                        Parser.succeed (Absolute North)

                    "S" ->
                        Parser.succeed (Absolute South)

                    "E" ->
                        Parser.succeed (Absolute East)

                    "W" ->
                        Parser.succeed (Absolute West)

                    "L" ->
                        Parser.succeed (Relative Left)

                    "R" ->
                        Parser.succeed (Relative Right)

                    "F" ->
                        Parser.succeed (Relative Forward)

                    _ ->
                        Parser.problem ("with invalid direction: " ++ str)
            )
