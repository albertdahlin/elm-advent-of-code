module Util.Parser exposing (..)

import Parser exposing ((|.), (|=), Parser)


run : Parser a -> String -> Result String a
run parser str =
    Parser.run parser str
        |> Result.mapError firstErrorMsg


firstErrorMsg : List Parser.DeadEnd -> String
firstErrorMsg list =
    errorMsg list
        |> List.head
        |> Maybe.withDefault "No Error"


errorMsg : List Parser.DeadEnd -> List String
errorMsg list =
    List.map
        (\deadEnd ->
            "Parse error at row "
                ++ String.fromInt deadEnd.row
                ++ ", col "
                ++ String.fromInt deadEnd.col
                ++ ": "
                ++ problemToString deadEnd.problem
        )
        list


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        Parser.Expecting str ->
            "Expecting " ++ str

        Parser.ExpectingInt ->
            "Expecting Int"

        Parser.ExpectingHex ->
            "Expecting Hex"

        Parser.ExpectingOctal ->
            "Expecting Octal"

        Parser.ExpectingBinary ->
            "Expecting Binary"

        Parser.ExpectingFloat ->
            "Expecting Float"

        Parser.ExpectingNumber ->
            "Expecting Number"

        Parser.ExpectingVariable ->
            "Expecting Variable"

        Parser.ExpectingSymbol str ->
            "Expecting symbol " ++ str

        Parser.ExpectingKeyword str ->
            "Expecting keyword " ++ str

        Parser.ExpectingEnd ->
            "Expecting End"

        Parser.UnexpectedChar ->
            "Unexpected Char"

        Parser.Problem str ->
            "Problem " ++ str

        Parser.BadRepeat ->
            "Bad Repeat"


parseRowsUsing : Parser a -> Parser (List a)
parseRowsUsing parser =
    Parser.loop []
        (\xs ->
            Parser.oneOf
                [ Parser.succeed (\x -> Parser.Loop (x :: xs))
                    |= parser
                    |. Parser.chompWhile (\c -> c == '\n' || c == '\u{000D}')
                , Parser.succeed (Parser.Done <| List.reverse xs)
                ]
        )


alpha : Parser String
alpha =
    Parser.succeed ()
        |. Parser.chompIf Char.isAlpha
        |. Parser.chompWhile Char.isAlpha
        |> Parser.getChompedString
