module Year2020.Day11 exposing (..)

import Performance exposing (Performance)
import Util.Mat2d as Mat
import Util.Vec2 as Vec2


type alias Grid =
    Mat.Mat2d Seat


solution =
    { solve = solve
    , title = "Seating System"
    , subtitle = "Solve \"Game of Life\" on the ferry seats."
    , tests = []
    , performance = Performance.Acceptable
    }


type Seat
    = Free
    | Occupied
    | Floor


solve : String -> ( Result String String, Result String String )
solve input =
    let
        grid =
            parse input

        r1 =
            grid
                |> untilStable tick 9999999
                |> String.fromInt
                |> Ok

        r2 =
            grid
                |> untilStable tick2 9999999
                |> String.fromInt
                |> Ok
    in
    ( r1
    , r2
    )


print : Grid -> List String
print grid =
    grid
        |> Mat.map
            (\_ seat ->
                case seat of
                    Free ->
                        'L'

                    Occupied ->
                        '#'

                    Floor ->
                        '.'
            )
        |> Mat.rows
        |> List.map String.fromList
        |> List.foldl (\r _ -> Debug.log "g" r) ""
        |> always []


untilStable : (Grid -> Grid) -> Int -> Grid -> Int
untilStable fn prev grid =
    let
        next =
            fn grid

        occupied =
            countOccupied next
    in
    if prev == occupied then
        occupied

    else
        untilStable fn occupied next


tick : Grid -> Grid
tick grid =
    Mat.map
        (\pos seat ->
            let
                around =
                    Mat.around pos grid

                occupied =
                    around
                        |> List.filter ((==) Occupied)
                        |> List.length
            in
            if seat == Free && occupied == 0 then
                Occupied

            else if seat == Occupied && occupied >= 4 then
                Free

            else
                seat
        )
        grid


countOccupied : Grid -> Int
countOccupied grid =
    Mat.foldl
        (\_ seat count ->
            if seat == Occupied then
                count + 1

            else
                count
        )
        0
        grid


tick2 : Grid -> Grid
tick2 grid =
    Mat.map
        (\pos seat ->
            let
                around =
                    [ visibleSeats pos ( -1, -1 ) grid
                    , visibleSeats pos ( -1, 0 ) grid
                    , visibleSeats pos ( -1, 1 ) grid
                    , visibleSeats pos ( 0, -1 ) grid
                    , visibleSeats pos ( 0, 1 ) grid
                    , visibleSeats pos ( 1, -1 ) grid
                    , visibleSeats pos ( 1, 0 ) grid
                    , visibleSeats pos ( 1, 1 ) grid
                    ]

                occupied =
                    around
                        |> List.filter ((==) Occupied)
                        |> List.length
            in
            if seat == Free && occupied == 0 then
                Occupied

            else if seat == Occupied && occupied >= 5 then
                Free

            else
                seat
        )
        grid


visibleSeats : ( Int, Int ) -> ( Int, Int ) -> Grid -> Seat
visibleSeats pos diff grid =
    let
        nextPos =
            Vec2.add pos diff
    in
    case Mat.get nextPos grid of
        Just Free ->
            Free

        Just Occupied ->
            Occupied

        Just Floor ->
            visibleSeats nextPos diff grid

        Nothing ->
            Floor


test =
    """L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"""
        |> parse



-- PARSE


parse : String -> Grid
parse input =
    let
        lines =
            String.lines input

        height =
            List.length lines

        width =
            List.head lines
                |> Maybe.map String.length
                |> Maybe.withDefault 0
    in
    Mat.forLinesAndChars
        input
        (\pos char grid ->
            case char of
                'L' ->
                    Mat.set pos Free grid

                '#' ->
                    Mat.set pos Occupied grid

                _ ->
                    Mat.set pos Floor grid
        )
        (Mat.init ( width, height ) Floor)
