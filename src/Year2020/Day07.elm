module Year2020.Day07 exposing (..)

import Dict exposing (Dict)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Result.Extra as Result
import Set exposing (Set)
import Util.Parser


solution =
    { solve = solve
    , title = "Handy Haversacks"
    , subtitle = "Find which luggage bags can contain \"shiny gold bags\"."
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Color =
    String


type alias Rule =
    ( Color, List ( Int, Color ) )


solve : String -> ( Result String String, Result String String )
solve input =
    let
        rules : Result String (List Rule)
        rules =
            parse input

        r1 =
            rules
                |> Result.map
                    (findAllContaining "shiny gold"
                        >> Set.size
                        >> String.fromInt
                    )

        r2 =
            rules
                |> Result.map
                    (Dict.fromList
                        >> countBagsWithin "shiny gold"
                        >> String.fromInt
                    )
    in
    ( r1
    , r2
    )


countBagsWithin : Color -> Dict String (List ( Int, String )) -> Int
countBagsWithin bagColor rules =
    case Dict.get bagColor rules of
        Just bagContent ->
            List.foldl
                (\( qty, color ) prev ->
                    prev + qty * (1 + countBagsWithin color rules)
                )
                0
                bagContent

        Nothing ->
            0


findAllContaining : String -> List Rule -> Set String
findAllContaining thisBag rules =
    flipRules rules
        |> findAllContainingHelper thisBag
        |> Tuple.first
        |> Set.remove thisBag


findAllContainingHelper : String -> Dict String (List String) -> ( Set String, Dict String (List String) )
findAllContainingHelper thisBag whereBagsAreContained =
    case Dict.get thisBag whereBagsAreContained of
        Just bagsContainingThisBag ->
            List.foldl
                (\bag ( set, dict ) ->
                    let
                        ( set2, dict2 ) =
                            findAllContainingHelper bag dict
                    in
                    ( Set.union set set2
                    , dict2
                    )
                )
                ( Set.singleton thisBag
                , Dict.remove thisBag whereBagsAreContained
                )
                bagsContainingThisBag

        Nothing ->
            ( Set.singleton thisBag, whereBagsAreContained )


flipRules : List Rule -> Dict Color (List Color)
flipRules rules =
    List.foldl
        (\( color, list ) dict ->
            List.foldl
                (\( _, color2 ) ->
                    Dict.update
                        color2
                        (\mb ->
                            case mb of
                                Just cl ->
                                    Just (color :: cl)

                                Nothing ->
                                    Just [ color ]
                        )
                )
                dict
                list
        )
        Dict.empty
        rules



-- PARSER


parse : String -> Result String (List Rule)
parse str =
    Parser.run parseRules str
        |> Result.mapError Util.Parser.firstErrorMsg


parseRules : Parser (List Rule)
parseRules =
    Parser.loop []
        (\xs ->
            Parser.oneOf
                [ Parser.succeed (\x -> Parser.Loop (x :: xs))
                    |= parseRule
                    |. Parser.spaces
                , Parser.succeed (Parser.Done xs)
                ]
        )


parseRule : Parser Rule
parseRule =
    Parser.succeed (\color list -> ( color, list ))
        |= parseColor
        |. Parser.spaces
        |. parseBags
        |. Parser.spaces
        |. parseContain
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed identity
                |= parseEmptyQualifier
                |. Parser.symbol "."
            , Parser.loop []
                (\xs ->
                    parseQualifier
                        |> Parser.andThen
                            (\x ->
                                Parser.oneOf
                                    [ Parser.succeed (Parser.Loop (x :: xs))
                                        |. Parser.symbol ","
                                        |. Parser.spaces
                                    , Parser.succeed (Parser.Done (x :: xs))
                                        |. Parser.symbol "."
                                    ]
                            )
                )
            ]


parseQualifier : Parser ( Int, String )
parseQualifier =
    Parser.succeed (\qty color -> ( qty, color ))
        |= Parser.int
        |. Parser.spaces
        |= parseColor
        |. Parser.spaces
        |. parseBags


parseEmptyQualifier : Parser (List a)
parseEmptyQualifier =
    Parser.succeed []
        |. Parser.keyword "no"
        |. Parser.spaces
        |. Parser.keyword "other"
        |. Parser.spaces
        |. Parser.keyword "bags"


parseContain : Parser ()
parseContain =
    Parser.keyword "contain"


parseBags : Parser ()
parseBags =
    Parser.oneOf
        [ Parser.keyword "bags"
        , Parser.keyword "bag"
        ]


parseColor : Parser String
parseColor =
    Parser.succeed (\adj color -> adj ++ " " ++ color)
        |= parseWord
        |. Parser.spaces
        |= parseWord


parseWord : Parser String
parseWord =
    Parser.getChompedString
        (Parser.succeed ()
            |. Parser.chompIf (not << isSpace)
            |. Parser.chompWhile (not << isSpace)
        )


isSpace : Char -> Bool
isSpace c =
    c == ' '
