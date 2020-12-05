module Year2015.Day07 exposing (..)

import Bitwise
import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Result.Extra as Result
import Set exposing (Set)


solution =
    { solve = solve
    , title = "Some Assembly Required"
    , subtitle = ""
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        instr =
            Parser.run parser input
                |> Result.mapError Debug.toString

        r1 =
            instr
                |> Result.map
                    (\ins ->
                        let
                            env =
                                Dict.fromList ins
                        in
                        eval (Var "a") env
                            |> Tuple.first
                    )

        r2 =
            Result.map2
                (\ins valueOfA ->
                    let
                        env =
                            Dict.fromList ins
                                |> Dict.insert "b" valueOfA
                    in
                    eval (Var "a") env
                        |> Tuple.first
                )
                instr
                r1

        format =
            Result.map
                (toInt
                    >> Maybe.map (\i -> String.fromInt i ++ " is provided to wire \"a\"")
                    >> Maybe.withDefault "Could not evaluate"
                )
    in
    ( format r1
    , format r2
    )



-- EVAL EXPR


type Expr
    = Val Int
    | Var String
    | And Expr Expr
    | Or Expr Expr
    | RShift Int Expr
    | LShift Int Expr
    | Not Expr


type alias Env =
    Dict String Expr


eval : Expr -> Env -> ( Expr, Env )
eval expr env =
    case expr of
        Val i ->
            ( Val i, env )

        Var key ->
            case Dict.get key env of
                Just ex ->
                    let
                        ( expr1, env1 ) =
                            eval ex env
                    in
                    ( expr1, Dict.insert key expr1 env1 )

                Nothing ->
                    ( expr, env )

        And e1 e2 ->
            let
                ( r1, env1 ) =
                    eval e1 env

                ( r2, env2 ) =
                    eval e2 env1
            in
            ( Maybe.map2
                (\a b -> Bitwise.and a b |> Val)
                (toInt r1)
                (toInt r2)
                |> Maybe.withDefault expr
            , env2
            )

        Or e1 e2 ->
            let
                ( r1, env1 ) =
                    eval e1 env

                ( r2, env2 ) =
                    eval e2 env1
            in
            ( Maybe.map2
                (\a b -> Bitwise.or a b |> Val)
                (toInt r1)
                (toInt r2)
                |> Maybe.withDefault expr
            , env2
            )

        RShift n e1 ->
            eval e1 env
                |> Tuple.mapFirst
                    (toInt
                        >> Maybe.map (Bitwise.shiftRightBy n >> toInt16 >> Val)
                        >> Maybe.withDefault expr
                    )

        LShift n e1 ->
            eval e1 env
                |> Tuple.mapFirst
                    (toInt
                        >> Maybe.map (Bitwise.shiftLeftBy n >> toInt16 >> Val)
                        >> Maybe.withDefault expr
                    )

        Not ex ->
            eval ex env
                |> Tuple.mapFirst
                    (toInt
                        >> Maybe.map (Bitwise.complement >> toInt16 >> Val)
                        >> Maybe.withDefault expr
                    )


toInt : Expr -> Maybe Int
toInt expr =
    case expr of
        Val i ->
            Just i

        _ ->
            Nothing


toInt16 : Int -> Int
toInt16 =
    modBy 65536



-- PARSE


parser : Parser (List ( String, Expr ))
parser =
    Parser.loop []
        (\xs ->
            Parser.oneOf
                [ Parser.succeed (\x -> Parser.Loop (x :: xs))
                    |= parseExpr
                    |. Parser.spaces
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse xs))
                ]
        )


parseExpr : Parser ( String, Expr )
parseExpr =
    Parser.oneOf
        [ parseNot
            |> Parser.andThen parseAssign
        , parseValOrVar
            |> Parser.andThen
                (\expr ->
                    Parser.oneOf
                        [ parseAssign expr
                        , parseBinOp expr
                            |> Parser.andThen parseAssign
                        ]
                )
        ]


parseAssign : Expr -> Parser ( String, Expr )
parseAssign expr =
    Parser.succeed (\key -> ( key, expr ))
        |. Parser.symbol "->"
        |. Parser.spaces
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isLower
            , reserved = Set.empty
            }


parseNot : Parser Expr
parseNot =
    Parser.succeed Not
        |. Parser.keyword "NOT"
        |. Parser.spaces
        |= Parser.lazy (\_ -> parseValOrVar)
        |. Parser.spaces


parseVal : Parser Expr
parseVal =
    Parser.succeed Val
        |= Parser.int
        |. Parser.spaces


parseVar : Parser Expr
parseVar =
    Parser.succeed Var
        |= Parser.variable
            { start = Char.isLower
            , inner = Char.isLower
            , reserved = Set.empty
            }
        |. Parser.spaces


parseValOrVar : Parser Expr
parseValOrVar =
    Parser.oneOf
        [ parseVar
        , parseVal
        ]


parseBinOp : Expr -> Parser Expr
parseBinOp expr =
    Parser.oneOf
        [ parseAnd expr
        , parseOr expr
        , parseRShift expr
        , parseLShift expr
        ]


parseAnd : Expr -> Parser Expr
parseAnd expr =
    Parser.succeed (And expr)
        |. Parser.keyword "AND"
        |. Parser.spaces
        |= parseValOrVar
        |. Parser.spaces


parseOr : Expr -> Parser Expr
parseOr expr =
    Parser.succeed (Or expr)
        |. Parser.keyword "OR"
        |. Parser.spaces
        |= parseValOrVar
        |. Parser.spaces


parseRShift : Expr -> Parser Expr
parseRShift expr =
    Parser.succeed (\n -> RShift n expr)
        |. Parser.keyword "RSHIFT"
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces


parseLShift : Expr -> Parser Expr
parseLShift expr =
    Parser.succeed (\n -> LShift n expr)
        |. Parser.keyword "LSHIFT"
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
