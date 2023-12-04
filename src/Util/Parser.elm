module Util.Parser exposing (..)

import Parser exposing ((|.), (|=), Parser)


run : Parser a -> String -> Result String a
run parser str =
    Parser.run parser str
        |> Result.mapError firstErrorMsg


firstErrorMsg : List Parser.DeadEnd -> String
firstErrorMsg =
    errorMsg
        >> List.head
        >> Maybe.withDefault "No Error"


errorMsg : List Parser.DeadEnd -> List String
errorMsg =
    List.map
        (\deadEnd ->
            "Parse error at row "
                ++ String.fromInt deadEnd.row
                ++ ", col "
                ++ String.fromInt deadEnd.col
                ++ ": "
                ++ problemToString deadEnd.problem
        )


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


lines : Parser a -> Parser (List a)
lines parser =
    Parser.loop []
        (\xs ->
            Parser.oneOf
                [ Parser.succeed (\x -> Parser.Loop (x :: xs))
                    |= parser
                    |. Parser.chompWhile (\c -> c == '\n' || c == '\u{000D}')
                , Parser.succeed (Parser.Done <| List.reverse xs)
                ]
        )


listSeparatedBy : Parser () -> Parser a -> Parser (List a)
listSeparatedBy separator parser =
    Parser.loop []
        (\xs ->
            parser
                |> Parser.andThen
                    (\x ->
                        Parser.oneOf
                            [ separator
                                |> Parser.map
                                    (\_ -> Parser.Loop (x :: xs))
                            , Parser.succeed
                                (Parser.Done <| List.reverse <| x :: xs)
                            ]
                    )
        )


listWhile : Parser a -> Parser (List a)
listWhile parser =
    Parser.loop []
        (\xs ->
            Parser.oneOf
                [ parser
                    |> Parser.map
                        (\x -> Parser.Loop (x :: xs))
                , Parser.succeed
                    (Parser.Done <| List.reverse xs)
                ]
        )


alpha : Parser String
alpha =
    Parser.succeed ()
        |. Parser.chompIf Char.isAlpha
        |. Parser.chompWhile Char.isAlpha
        |> Parser.getChompedString


newLine : Parser ()
newLine =
    Parser.chompIf (\c -> c == '\n')


atLeastOneOf : List Char -> Parser ()
atLeastOneOf chars =
    Parser.succeed ()
        |. Parser.chompIf (\c -> List.member c chars)
        |. Parser.chompWhile (\c -> List.member c chars)


int : Parser Int
int =
    Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen
            (\str ->
                case String.toInt str of
                    Just i ->
                        Parser.succeed i

                    Nothing ->
                        Parser.problem "Not an int"
            )
