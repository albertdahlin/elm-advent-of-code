module Year2020.Day04 exposing (..)

import Dict exposing (Dict)
import Performance exposing (Performance)
import Regex
import Set exposing (Set)


solution =
    { solve = solve
    , title = "Passport Processing"
    , subtitle = "\"Help\" airport security validating \"passports\"."
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Passport =
    Dict String String


solve : String -> ( Result String String, Result String String )
solve input =
    let
        passports =
            String.split "\n\n" input
                |> List.map parsePassport

        r1 =
            List.filter hasRequiredFields passports
                |> List.length
                |> (\l -> String.fromInt l ++ " \"passports\" has all required fields")
                |> Ok

        r2 =
            List.filter isValid passports
                |> List.length
                |> (\l -> String.fromInt l ++ " valid \"passports\"")
                |> Ok
    in
    ( r1
    , r2
    )


requiredFields : List String
requiredFields =
    [ "byr"
    , "iyr"
    , "eyr"
    , "hgt"
    , "hcl"
    , "ecl"
    , "pid"
    ]


hasRequiredFields : Passport -> Bool
hasRequiredFields passport =
    List.all (\f -> Dict.member f passport) requiredFields


fieldValidation : List ( String, String -> Bool )
fieldValidation =
    [ ( "byr", intBetween 1920 2002 )
    , ( "iyr", intBetween 2010 2020 )
    , ( "eyr", intBetween 2020 2030 )
    , ( "hgt"
      , \v ->
            case String.right 2 v of
                "cm" ->
                    intBetween 150 193 (String.dropRight 2 v)

                "in" ->
                    intBetween 59 76 (String.dropRight 2 v)

                _ ->
                    False
      )
    , ( "hcl", isHexColor )
    , ( "ecl", \v -> Set.member v validEyeColors )
    , ( "pid", isValidPassportId )
    ]


intBetween : Int -> Int -> String -> Bool
intBetween low high v =
    String.toInt v
        |> Maybe.map (\y -> y >= low && y <= high)
        |> Maybe.withDefault False


isHexColor : String -> Bool
isHexColor =
    Regex.contains
        (Regex.fromString "^#[\\da-f]{6}$"
            |> Maybe.withDefault Regex.never
        )


isValidPassportId : String -> Bool
isValidPassportId =
    Regex.contains
        (Regex.fromString "^\\d{9}$"
            |> Maybe.withDefault Regex.never
        )


validEyeColors : Set String
validEyeColors =
    Set.fromList [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]


isValid : Passport -> Bool
isValid passport =
    List.all
        (\( key, fn ) ->
            case Dict.get key passport of
                Just val ->
                    fn val

                Nothing ->
                    False
        )
        fieldValidation


parsePassport : String -> Passport
parsePassport str =
    String.words str
        |> List.foldl
            (\kv ->
                case String.split ":" kv of
                    [ k, v ] ->
                        Dict.insert k v

                    _ ->
                        identity
            )
            Dict.empty
