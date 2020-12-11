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
