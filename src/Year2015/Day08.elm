module Year2015.Day08 exposing (..)

import Hex
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Result.Extra as Result
import Util.Parser


solution =
    { solve = solve
    , title = "Matchsticks"
    , subtitle = "Escape and unescape strings."
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        rows =
            String.lines input

        unEscapedRows =
            List.map unEscape rows
                |> Result.combine

        rowsCharCount =
            List.foldl (String.length >> (+)) 0 rows

        charsInMem =
            unEscapedRows
                |> Result.map
                    (List.foldl (String.length >> (+)) 0)

        r1 =
            Result.map
                (\inMem ->
                    rowsCharCount
                        - inMem
                        |> String.fromInt
                )
                charsInMem

        escapedCharsCount =
            List.foldl (escape >> String.length >> (+)) 0 rows

        r2 =
            escapedCharsCount
                - rowsCharCount
                |> String.fromInt
                |> Ok
    in
    ( r1
    , r2
    )


escape : String -> String
escape str =
    "\"" ++ (String.toList str |> escapeHelp |> String.fromList) ++ "\""


escapeHelp : List Char -> List Char
escapeHelp list =
    case list of
        [] ->
            []

        char :: rest ->
            case char of
                '"' ->
                    '\\' :: char :: escapeHelp rest

                '\\' ->
                    '\\' :: char :: escapeHelp rest

                _ ->
                    char :: escapeHelp rest



-- PARSER


unEscape : String -> Result String String
unEscape str =
    Parser.run parseStringLit str
        |> Result.mapError Util.Parser.firstErrorMsg


parseStringLit : Parser String
parseStringLit =
    Parser.succeed identity
        |. chompChar '"'
        |= Parser.loop ""
            (\str ->
                Parser.oneOf
                    [ Parser.succeed (\s -> Parser.Loop (str ++ s))
                        |= parseUntilEscape
                    , Parser.succeed (\s -> Parser.Loop (str ++ s))
                        |= parseEscaped
                    , Parser.succeed (Parser.Done str)
                    ]
            )
        |. chompChar '"'


parseUntilEscape : Parser String
parseUntilEscape =
    Parser.getChompedString
        (Parser.succeed ()
            |. Parser.chompIf (not << charNeedsEscape)
            |. Parser.chompWhile (not << charNeedsEscape)
        )


charNeedsEscape : Char -> Bool
charNeedsEscape c =
    c == '\\' || c == '"'


chompChar : Char -> Parser ()
chompChar char =
    Parser.chompIf (\c -> c == char)


chompHexDigit : Parser ()
chompHexDigit =
    Parser.chompIf (\c -> Char.isHexDigit c)


chompOneChar : Parser ()
chompOneChar =
    Parser.chompIf (\_ -> True)


parseEscaped : Parser String
parseEscaped =
    Parser.succeed identity
        |. chompChar '\\'
        |= Parser.oneOf
            [ parseBackslashOrQuote
            , parseHexAscii
            , Parser.getChompedString chompOneChar
                |> Parser.map ((++) "\\")
            ]


parseBackslashOrQuote : Parser String
parseBackslashOrQuote =
    Parser.getChompedString
        (Parser.chompIf charNeedsEscape)


parseHexAscii : Parser String
parseHexAscii =
    Parser.getChompedString
        (Parser.succeed ()
            |. chompChar 'x'
            |. chompHexDigit
            |. chompHexDigit
        )
        |> Parser.andThen
            (\hex ->
                case Hex.fromString (String.dropLeft 1 hex) of
                    Ok xx ->
                        Char.fromCode xx
                            |> String.fromChar
                            |> Parser.succeed

                    Err e ->
                        Parser.problem e
            )
