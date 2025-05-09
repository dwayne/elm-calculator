module Lib.Eval exposing
    ( Eval
    , actOnState
    , andThen
    , fail
    , followedBy
    , modifyState
    , run
    , succeed
    )


type Eval e s a
    = Eval (s -> ( Result e a, s ))


succeed : a -> Eval e s a
succeed value =
    Eval
        (\s0 ->
            ( Ok value, s0 )
        )


fail : e -> Eval e s a
fail e =
    Eval
        (\s0 ->
            ( Err e, s0 )
        )


run : s -> Eval e s a -> Result e a
run s =
    apply s >> Tuple.first


followedBy : Eval e s b -> Eval e s a -> Eval e s b
followedBy eval2 eval1 =
    eval1
        |> andThen (\_ -> eval2)


andThen : (a -> Eval e s b) -> Eval e s a -> Eval e s b
andThen mf (Eval st) =
    Eval
        (\s0 ->
            case st s0 of
                ( Ok value, s1 ) ->
                    apply s1 (mf value)

                ( Err e, s1 ) ->
                    ( Err e, s1 )
        )


modifyState : (s -> s) -> Eval e s ()
modifyState f =
    Eval
        (\s0 ->
            ( Ok (), f s0 )
        )


actOnState : (s -> ( Eval e s a, s )) -> Eval e s a
actOnState f =
    Eval
        (\s0 ->
            let
                ( eval, s1 ) =
                    f s0
            in
            apply s1 eval
        )


apply : s -> Eval e s a -> ( Result e a, s )
apply s (Eval st) =
    st s
