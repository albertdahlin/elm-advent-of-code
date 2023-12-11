module Util.Grid exposing (..)

import Dict exposing (Dict)


type alias Grid a =
    { width : Int
    , height : Int
    , cells : Dict ( Int, Int ) a
    }


parse : (Char -> Maybe a) -> String -> Grid a
parse fn input =
    let
        lines =
            String.lines input

        height =
            List.length lines

        width =
            List.head lines
                |> Maybe.map String.length
                |> Maybe.withDefault 0

        loopCols row col listOfChar dict =
            case listOfChar of
                [] ->
                    dict

                char :: rest ->
                    (case fn char of
                        Just a ->
                            Dict.insert ( row, col ) a dict

                        Nothing ->
                            dict
                    )
                        |> loopCols
                            row
                            (col + 1)
                            rest

        loopRows r rows dict =
            case rows of
                [] ->
                    dict

                row :: rest ->
                    loopRows
                        (r + 1)
                        rest
                        (loopCols r 0 (String.toList row) dict)
    in
    { width = width
    , height = height
    , cells = loopRows 0 lines Dict.empty
    }


print : Grid a -> (( Int, Int ) -> Maybe a -> String) -> String
print grid cellFn =
    List.range 0 (grid.height - 1)
        |> List.map
            (\row ->
                List.range 0 (grid.width - 1)
                    |> List.map
                        (\col ->
                            let
                                pos =
                                    ( row, col )
                            in
                            Dict.get pos grid.cells
                                |> cellFn pos
                        )
                    |> String.join ""
            )
        |> String.join "\n"
        |> (++) "\n"


fold : (( Int, Int ) -> Maybe a -> b -> b) -> b -> Grid a -> b
fold fn init grid =
    List.range 0 grid.height
        |> List.foldl
            (\row state1 ->
                List.range 0 grid.width
                    |> List.foldl
                        (\col state2 ->
                            let
                                pos =
                                    ( row, col )
                            in
                            fn
                                pos
                                (Dict.get pos grid.cells)
                                state2
                        )
                        state1
            )
            init


adjacent : ( Int, Int ) -> List ( Int, Int )
adjacent ( x, y ) =
    [ ( x - 1, y )
    , ( x - 1, y + 1 )
    , ( x - 1, y - 1 )
    , ( x + 1, y )
    , ( x + 1, y + 1 )
    , ( x + 1, y - 1 )
    , ( x, y + 1 )
    , ( x, y - 1 )
    ]


adjacent4 : ( Int, Int ) -> List ( Int, Int )
adjacent4 ( x, y ) =
    [ ( x - 1, y )
    , ( x + 1, y )
    , ( x, y + 1 )
    , ( x, y - 1 )
    ]


getAdjacentFrom : Dict ( Int, Int ) a -> ( Int, Int ) -> List a
getAdjacentFrom dict pos =
    getAdjacent pos dict


getAdjacent : ( Int, Int ) -> Dict ( Int, Int ) a -> List a
getAdjacent ( x, y ) dict =
    [ Dict.get ( x - 1, y + 1 ) dict
    , Dict.get ( x - 1, y ) dict
    , Dict.get ( x - 1, y - 1 ) dict
    , Dict.get ( x + 1, y + 1 ) dict
    , Dict.get ( x + 1, y ) dict
    , Dict.get ( x + 1, y - 1 ) dict
    , Dict.get ( x, y + 1 ) dict
    , Dict.get ( x, y - 1 ) dict
    ]
        |> List.filterMap identity


getDirRowCol ( row, col ) =
    if col < 0 && row == 0 then
        Just Left

    else if col > 0 && row == 0 then
        Just Right

    else if col == 0 && row < 0 then
        Just Up

    else if col == 0 && row > 0 then
        Just Down

    else
        Nothing


getDir ( x, y ) =
    if x == -1 && y == 0 then
        Just Left

    else if x == 1 && y == 0 then
        Just Right

    else if x == 0 && y == -1 then
        Just Down

    else if x == 0 && y == 1 then
        Just Up

    else
        Nothing


rowColFrom dir =
    case dir of
        Up ->
            ( -1, 0 )

        Down ->
            ( 1, 0 )

        Right ->
            ( 0, 1 )

        Left ->
            ( 0, -1 )


type Dir
    = Up
    | Down
    | Right
    | Left


turnCW dir =
    case dir of
        Up ->
            Right

        Down ->
            Left

        Right ->
            Down

        Left ->
            Up


turnCCW dir =
    case dir of
        Up ->
            Left

        Down ->
            Right

        Right ->
            Up

        Left ->
            Down


turn : Turn -> Dir -> Dir
turn t d =
    case t of
        CW ->
            turnCW d

        CCW ->
            turnCCW d


getTurn dir1 dir2 =
    case ( dir1, dir2 ) of
        ( Right, Down ) ->
            Just CW

        ( Right, Up ) ->
            Just CCW

        ( Down, Left ) ->
            Just CW

        ( Down, Right ) ->
            Just CCW

        ( Left, Up ) ->
            Just CW

        ( Left, Down ) ->
            Just CCW

        ( Up, Right ) ->
            Just CW

        ( Up, Left ) ->
            Just CCW

        _ ->
            Nothing


type Turn
    = CW
    | CCW


length ( x, y ) =
    abs x + abs y
