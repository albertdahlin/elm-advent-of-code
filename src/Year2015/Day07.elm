module Year2015.Day07 exposing (..)

import Bitwise
import Dict exposing (Dict)
import Monad.Result.State as State
import Parser exposing ((|.), (|=), Parser)
import Performance exposing (Performance)
import Set exposing (Set)
import Util.Parser


solution =
    { solve = solve
    , title = "Some Assembly Required"
    , subtitle = "Help Bobby Tables to connect his logic gates"
    , tests = []
    , performance = Performance.Acceptable
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        listOfInstructions =
            Parser.run parser input
                |> Result.mapError Util.Parser.firstErrorMsg

        r1 =
            listOfInstructions
                |> Result.andThen
                    (\list ->
                        let
                            env =
                                Dict.fromList list
                        in
                        evalToInt (Var "a")
                            |> State.finalValue env
                    )

        r2 =
            Result.map2
                (\list valueOfA ->
                    let
                        env =
                            Dict.fromList list
                                |> Dict.insert "b" (Val valueOfA)
                    in
                    evalToInt (Var "a")
                        |> State.finalValue env
                )
                listOfInstructions
                r1
                |> Result.andThen identity

        format =
            Result.map
                (\i -> String.fromInt i ++ " is provided to wire \"a\"")
    in
    ( format r1
    , format r2
    )



-- EVAL EXPR


type Expr
    = Val Int
    | Var String
    | BinOp BinOp Expr Expr
    | Not Expr


type BinOp
    = And
    | Or
    | RShift
    | LShift


type alias Env =
    Dict String Expr


{-| State Monad wrapped in Result
-}
type alias StateMonad a =
    State.State String Env a

{-|
Evaluate starting at an expression (Var "a").

Whenever a variable lookup is evaluated (Var <name>) the Env dict is
updated with the result to avoid evaluating the same expr multiple times.
This improves performance a lot.

This is solved using a state monad wrapped in Result which
threads `Env` through all computations and also handles the case
when an expr can not be evaluated to `Int`.
-}
evalToInt : Expr -> StateMonad Int
evalToInt expr =
    case expr of
        Val i ->
            State.return i

        Var key ->
            State.get
                |> State.map (Dict.get key)
                |> State.andThen
                    (\mbExpr ->
                        case mbExpr of
                            Just ex ->
                                evalToInt ex
                                    |> State.update
                                        (\i env -> Dict.insert key (Val i) env)

                            Nothing ->
                                State.fail ("Undefined var: " ++ key)
                    )

        BinOp binOp e1 e2 ->
            State.map2
                (\a b ->
                    case binOp of
                        And ->
                            Bitwise.and a b
                                |> toInt16

                        Or ->
                            Bitwise.or a b
                                |> toInt16

                        RShift ->
                            Bitwise.shiftRightBy b a
                                |> toInt16

                        LShift ->
                            Bitwise.shiftLeftBy b a
                                |> toInt16
                )
                (evalToInt e1)
                (evalToInt e2)

        Not ex ->
            evalToInt ex
                |> State.map
                    (Bitwise.complement >> toInt16)


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
                        , Parser.oneOf
                            [ parseBinOp "AND" (BinOp And expr)
                            , parseBinOp "OR" (BinOp Or expr)
                            , parseBinOp "RSHIFT" (BinOp RShift expr)
                            , parseBinOp "LSHIFT" (BinOp LShift expr)
                            ]
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
        |= parseValOrVar
        |. Parser.spaces


parseValOrVar : Parser Expr
parseValOrVar =
    Parser.oneOf
        [ Parser.succeed Var
            |= Parser.variable
                { start = Char.isLower
                , inner = Char.isLower
                , reserved = Set.empty
                }
            |. Parser.spaces
        , Parser.succeed Val
            |= Parser.int
            |. Parser.spaces
        ]


parseBinOp : String -> (Expr -> Expr) -> Parser Expr
parseBinOp keyword toExpr =
    Parser.succeed toExpr
        |. Parser.keyword keyword
        |. Parser.spaces
        |= parseValOrVar
        |. Parser.spaces
