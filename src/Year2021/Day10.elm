module Year2021.Day10 exposing (..)

import Performance exposing (Performance)


solution =
    { solve = solve
    , title = "Syntax Scoring"
    , subtitle = "Determinate the score of currupted and incomplete syntax of the navigation subsystem."
    , tests = []
    , performance = Performance.Acceptable
    }


type ParseResult
    = Done (List Char)
    | Incomplete (List Char)
    | Error (List Char)


solve : String -> ( Result String String, Result String String )
solve input =
    let
        parsed =
            String.lines input
                |> List.map String.toList
                |> List.map parseSubsystem

        r1 =
            parsed
                |> List.filterMap
                    (\result ->
                        case result of
                            Done _ ->
                                Nothing

                            Incomplete _ ->
                                Nothing

                            Error chars ->
                                Just chars
                    )
                |> List.map (List.head >> Maybe.map points1 >> Maybe.withDefault 0)
                |> List.sum
                |> String.fromInt
                |> Ok

        r2 =
            parsed
                |> List.filterMap
                    (\result ->
                        case result of
                            Done _ ->
                                Nothing

                            Incomplete chars ->
                                Just chars

                            Error _ ->
                                Nothing
                    )
                |> List.map (List.map getClosingFor >> autoCompleteScore 0)
                |> median
                |> String.fromInt
                |> Ok
    in
    ( r1
    , r2
    )


isOpenChar : Char -> Bool
isOpenChar c =
    c == '(' || c == '[' || c == '{' || c == '<'


isCloseFor : Char -> Char -> Bool
isCloseFor open close =
    getClosingFor open == close


getClosingFor : Char -> Char
getClosingFor c =
    case c of
        '(' ->
            ')'

        '[' ->
            ']'

        '{' ->
            '}'

        '<' ->
            '>'

        _ ->
            c


parseSubsystem : List Char -> ParseResult
parseSubsystem list =
    case list of
        [] ->
            Done []

        x :: xs ->
            parseSubsystemHelp [ x ] x xs


parseSubsystemHelp : List Char -> Char -> List Char -> ParseResult
parseSubsystemHelp stack char list =
    case list of
        [] ->
            if List.isEmpty stack then
                Done []

            else
                Incomplete stack

        x :: xs ->
            if isOpenChar x then
                case parseSubsystemHelp (x :: stack) x xs of
                    Done chars ->
                        parseSubsystemHelp stack char chars

                    Incomplete chars ->
                        Incomplete chars

                    Error chars ->
                        Error chars

            else if isCloseFor char x then
                Done xs

            else
                Error list



-- SCORE


points1 : Char -> Int
points1 c =
    case c of
        ')' ->
            3

        ']' ->
            57

        '}' ->
            1197

        '>' ->
            25137

        _ ->
            0


autoCompleteScore : Int -> List Char -> Int
autoCompleteScore score list =
    case list of
        [] ->
            score

        x :: xs ->
            autoCompleteScore (score * 5 + points2 x) xs


points2 : Char -> Int
points2 c =
    case c of
        ')' ->
            1

        ']' ->
            2

        '}' ->
            3

        '>' ->
            4

        _ ->
            0


median : List Int -> Int
median list =
    List.sort list
        |> List.drop (List.length list // 2)
        |> List.head
        |> Maybe.withDefault 0
