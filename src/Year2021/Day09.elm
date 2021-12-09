module Year2021.Day09 exposing (..)

import Dict exposing (Dict)
import Performance exposing (Performance)
import Result.Extra as Result


solution =
    { solve = solve
    , title = "Smoke Basin"
    , subtitle = "Find low points and largest basins on an heightmap."
    , tests = []
    , performance = Performance.Acceptable
    }


type alias Grid =
    Dict ( Int, Int ) Int


solve : String -> ( Result String String, Result String String )
solve rawInput =
    let
        parsedGrid =
            parse rawInput
                |> Result.map (Dict.filter (\_ height -> height < 9))

        lowestPoints =
            parsedGrid
                |> Result.map findLowestPoints

        r1 =
            lowestPoints
                |> Result.map
                    (List.map (Tuple.second >> (+) 1)
                        >> List.sum
                        >> String.fromInt
                    )

        r2 =
            Result.map2
                (\lowPoints grid ->
                    lowPoints
                        |> List.map
                            (\( pos, height ) ->
                                findBasin pos height grid
                                    |> Dict.size
                            )
                        |> List.sort
                        |> List.reverse
                        |> List.take 3
                        |> List.product
                        |> String.fromInt
                )
                lowestPoints
                parsedGrid
    in
    ( r1
    , r2
    )


findLowestPoints : Grid -> List ( ( Int, Int ), Int )
findLowestPoints grid =
    grid
        |> Dict.foldl
            (\( row, col ) height lowPoints ->
                let
                    isLowest =
                        [ Dict.get ( row + 1, col ) grid
                        , Dict.get ( row - 1, col ) grid
                        , Dict.get ( row, col + 1 ) grid
                        , Dict.get ( row, col - 1 ) grid
                        ]
                            |> List.filterMap identity
                            |> List.all (\h -> height < h)
                in
                if isLowest then
                    ( ( row, col ), height ) :: lowPoints

                else
                    lowPoints
            )
            []


findBasin : ( Int, Int ) -> Int -> Grid -> Grid
findBasin pos height grid =
    Dict.singleton pos height
        |> searchAround pos height grid


searchAround : ( Int, Int ) -> Int -> Grid -> Grid -> Grid
searchAround (( row, col ) as pos) height grid result =
    Dict.insert pos height result
        |> repeatIfHigher ( row + 1, col ) height grid
        |> repeatIfHigher ( row - 1, col ) height grid
        |> repeatIfHigher ( row, col + 1 ) height grid
        |> repeatIfHigher ( row, col - 1 ) height grid


repeatIfHigher : ( Int, Int ) -> Int -> Grid -> Grid -> Grid
repeatIfHigher pos height grid result =
    case Dict.get pos grid of
        Just h ->
            if h > height then
                Dict.insert pos h result
                    |> searchAround pos h grid

            else
                result

        Nothing ->
            result



-- PARSE


parse : String -> Result String Grid
parse str =
    String.lines str
        |> List.indexedMap
            (\rowIdx row ->
                String.split "" row
                    |> List.indexedMap
                        (\colIdx digit ->
                            case String.toInt digit of
                                Just n ->
                                    Ok ( ( rowIdx, colIdx ), n )

                                Nothing ->
                                    Err ("Not a digit: " ++ digit)
                        )
            )
        |> List.concat
        |> Result.combine
        |> Result.map Dict.fromList
