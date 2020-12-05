module Util.Parser exposing (..)

import Parser


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
