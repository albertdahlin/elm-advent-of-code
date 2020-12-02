module Year2015.Day06 exposing (..)

import Result.Extra as Result
import Set exposing (Set)
import Array exposing (Array)
import Array.Extra as Array
import Util.Vec2 as Vec2


type alias Vec2 =
    Vec2.Vec2 Int


type alias Grid =
    Array Int


type Instr
    = TurnOn Vec2 Vec2
    | Toggle Vec2 Vec2
    | TurnOff Vec2 Vec2


solution =
    { solve = solve
    , title = "Probably a Fire Hazard"
    , tests = []
    }


solve : String -> ( Result String String, Result String String )
solve input =
    let
        instructions =
            String.lines input
                |> List.map parseLine
                |> Result.combine

        r1 =
            instructions
                |> Result.map
                    (List.foldl
                        (\instr grid2 ->
                            case instr of
                                TurnOn from to ->
                                    forEachPoint
                                        (\x y -> Array.set (x + y * 1000) 1)
                                        from
                                        to
                                        grid2

                                TurnOff from to ->
                                    forEachPoint
                                        (\x y -> Array.set (x + y * 1000) 0)
                                        from
                                        to
                                        grid2


                                Toggle from to ->
                                    forEachPoint
                                        (\x y -> Array.update (x + y * 1000) (\n -> if n == 0 then 1 else 0))
                                        from
                                        to
                                        grid2
                        )
                        (Array.repeat (1000*1000) 0)
                        >> Array.foldl (+) 0
                        >> (\sz -> String.fromInt sz)
                    )

        r2 =
            instructions
                |> Result.map
                    (List.foldl
                        (\instr grid2 ->
                            case instr of
                                TurnOn from to ->
                                    forEachPoint
                                        (\x y -> Array.update (x + y * 1000) ((+) 1))
                                        from
                                        to
                                        grid2

                                TurnOff from to ->
                                    forEachPoint
                                        (\x y -> Array.update (x + y * 1000) ((+) -1 >> max 0))
                                        from
                                        to
                                        grid2


                                Toggle from to ->
                                    forEachPoint
                                        (\x y -> Array.update (x + y * 1000) ((+) 2))
                                        from
                                        to
                                        grid2
                        )
                        (Array.repeat (1000*1000) 0)
                        >> Array.foldl (+) 0
                        >> (\sz -> String.fromInt sz)
                    )
    in
    ( r1
    , r2
    )


forEachPoint : (Int -> Int -> a -> a) -> Vec2 -> Vec2 -> a -> a
forEachPoint fn ( x1, y1 ) ( x2, y2 ) grid =
    let
        allY =
            List.range y1 y2
    in
    List.range x1 x2
        |> List.foldl
            (\x grid2 ->
                List.foldl
                    (\y ->
                        fn x y
                    )
                    grid2
                    allY
            )
            grid


parseLine : String -> Result String Instr
parseLine line =
    case String.split " " line of
        [ "turn", "on", from, "through", to ] ->
            Result.map2
                (\f t -> TurnOn f t)
                (parseVec from)
                (parseVec to)

        [ "toggle", from, "through", to ] ->
            Result.map2
                (\f t -> Toggle f t)
                (parseVec from)
                (parseVec to)

        [ "turn", "off", from, "through", to ] ->
            Result.map2
                (\f t -> TurnOff f t)
                (parseVec from)
                (parseVec to)

        _ ->
            Err ("Wrong format: " ++ line)


parseVec : String -> Result String Vec2
parseVec str =
    String.split "," str
        |> Vec2.fromList
        |> Maybe.andThen (Vec2.filterMap String.toInt)
        |> Result.fromMaybe "Not a vector"
