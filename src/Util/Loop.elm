module Util.Loop exposing (..)


type Loop state done
    = Step state
    | Done done


loop : state -> (state -> Loop state done) -> done
loop state fn =
    case fn state of
        Step next ->
            loop next fn

        Done d ->
            d


repeat : Int -> (a -> a) -> a -> a
repeat n fn a =
    if n == 0 then
        a

    else
        repeat (n - 1) fn (fn a)


repeatUntil : (a -> Bool) -> (a -> a) -> a -> a
repeatUntil isDone fn a =
    if isDone a then
        a

    else
        repeatUntil isDone fn (fn a)


repeatWhile : (a -> Bool) -> (a -> a) -> a -> a
repeatWhile isGood fn a =
    if isGood a then
        repeatWhile isGood fn (fn a)

    else
        a


untilNoChangeIn : (state -> result) -> (state -> state) -> state -> result
untilNoChangeIn toRes fn state =
    repeatUntilNoChangeHelp (toRes state) fn toRes (fn state)


repeatUntilNoChangeHelp prev fn toRes state =
    let
        nextState =
            fn state

        result =
            toRes nextState
    in
    if result == prev then
        result

    else
        repeatUntilNoChangeHelp result fn toRes nextState
