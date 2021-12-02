module Year2021.Day02 exposing (..)

import Performance exposing (Performance)
import Result.Extra as Result
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
            parseInput input

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


parseInput : String -> Result String (List Dir)
parseInput str =
    String.lines str
        |> List.map (String.split " " >> parseWords)
        |> Result.combine


parseWords : List String -> Result String Dir
parseWords words =
    case words of
        [ dir, n ] ->
            case String.toInt n of
                Just i ->
                    case dir of
                        "up" ->
                            Ok (Up i)

                        "down" ->
                            Ok (Down i)

                        "forward" ->
                            Ok (Forward i)

                        _ ->
                            Err "Only up, down, forward is allowed"

                _ ->
                    Err "Length is not an integer"

        _ ->
            Err "Wrong format on row"


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
