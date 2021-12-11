module Year2016.Day01 exposing (..)

import List.Extra as List
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Set exposing (Set)
import Util.Parser
import Util.Vec2 as Vec2


solution =
    { solve = solve
    , title = "No Time for a Taxicab"
    , subtitle = "Navigate a Manhattan grid following instructions."
    , tests = []
    , performance = Performance.Acceptable
    }


type Turn
    = Left Int
    | Right Int


{-| Direction as Int

0 - North
1 - East
2 - South
3 - West

-}
type alias Dir =
    Int


solve : String -> ( Result String String, Result String String )
solve input =
    let
        parsedTurns =
            Util.Parser.run parser input

        r1 =
            parsedTurns
                |> Result.map
                    (\turns ->
                        List.map toPolarVector turns
                            |> List.scanl applyTurn ( 0, 0 )
                            |> List.map fromPolar
                            |> List.foldl1 Vec2.add
                            |> Maybe.map (Vec2.manhattanDistance >> String.fromInt)
                            |> Maybe.withDefault ""
                    )

        r2 =
            parsedTurns
                |> Result.map
                    (\turns ->
                        List.map toPolarVector turns
                            |> List.scanl applyTurn ( 0, 0 )
                            |> List.concatMap toSteps
                            |> List.scanl1 Vec2.add
                            |> findFirstDuplicate
                            |> Maybe.map (Vec2.manhattanDistance >> String.fromInt)
                            |> Maybe.withDefault "No intersection"
                    )
    in
    ( r1
    , r2
    )


findFirstDuplicate : List comparable -> Maybe comparable
findFirstDuplicate list =
    findFirstDuplicateHelp Set.empty list


findFirstDuplicateHelp : Set comparable -> List comparable -> Maybe comparable
findFirstDuplicateHelp set list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if Set.member x set then
                Just x

            else
                findFirstDuplicateHelp (Set.insert x set) xs


toSteps : ( Int, Dir ) -> List ( Int, Int )
toSteps ( dist, dir ) =
    case dir of
        0 ->
            List.repeat dist ( 0, 1 )

        1 ->
            List.repeat dist ( 1, 0 )

        2 ->
            List.repeat dist ( 0, -1 )

        _ ->
            List.repeat dist ( -1, 0 )


{-| Turn polar vector to rectangular vector
-}
fromPolar : ( Int, Dir ) -> ( Int, Int )
fromPolar ( dist, dir ) =
    case dir of
        0 ->
            ( 0, dist )

        1 ->
            ( dist, 0 )

        2 ->
            ( 0, -dist )

        _ ->
            ( -dist, 0 )


applyTurn : ( Int, Dir ) -> ( Int, Int ) -> ( Int, Dir )
applyTurn ( dist, turn ) ( _, dir ) =
    ( dist
    , modBy 4 (dir + turn)
    )


toPolarVector : Turn -> ( Int, Int )
toPolarVector turn =
    case turn of
        Left dist ->
            ( dist, -1 )

        Right dist ->
            ( dist, 1 )



-- PARSER


parser : Parser (List Turn)
parser =
    Parser.sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = Parser.spaces
        , item = parserTurn
        , trailing = Parser.Optional
        }


parserTurn : Parser Turn
parserTurn =
    Parser.oneOf
        [ Parser.token "R"
            |> Parser.map (\_ -> Right)
        , Parser.token "L"
            |> Parser.map (\_ -> Left)
        ]
        |= Parser.int
