module Data.Evaluator exposing (Answer, Error(..), eval)

import Data.Operator exposing (Operator(..))
import Data.Token exposing (Token(..))
import Lib.Eval as Eval
import Lib.Rational as Rational exposing (Rational)
import Lib.Stack as Stack exposing (Stack)


type alias Answer =
    Result Error Rational


type alias Eval a =
    Eval.Eval Error State a


type Error
    = SyntaxError


type alias State =
    { operands : Stack Rational
    , operators : Stack Operator
    }


eval : List Token -> Answer
eval =
    --
    -- Evaluates infix expressions using Dijkstra's shunting yard algorithm.
    --
    evalTokens
        >> Eval.followedBy getValue
        >> Eval.run
            { operands = Stack.new
            , operators = Stack.new
            }


getValue : Eval Rational
getValue =
    Eval.actOnState
        (\state ->
            case ( Stack.pop state.operands, Stack.isEmpty state.operators ) of
                ( Just ( value, newOperands ), True ) ->
                    if Stack.isEmpty newOperands then
                        ( Eval.succeed value, { state | operands = newOperands } )

                    else
                        ( Eval.fail SyntaxError, state )

                _ ->
                    ( Eval.fail SyntaxError, state )
        )


evalTokens : List Token -> Eval ()
evalTokens tokens =
    case tokens of
        [] ->
            evalOperators

        token :: restTokens ->
            evalToken token
                |> Eval.followedBy (evalTokens restTokens)


evalToken : Token -> Eval ()
evalToken token =
    case token of
        Number n ->
            pushOperand n

        Operator op ->
            evalDominantOperators op


evalDominantOperators : Operator -> Eval ()
evalDominantOperators op1 =
    popOperator
        (\op2 ->
            if precedence op2 >= precedence op1 then
                evalOperation op2
                    |> Eval.followedBy (evalDominantOperators op1)

            else
                pushOperator op2
                    |> Eval.followedBy (pushOperator op1)
        )
        (pushOperator op1)


evalOperators : Eval ()
evalOperators =
    popOperator
        (\op ->
            evalOperation op
                |> Eval.followedBy evalOperators
        )
        (Eval.succeed ())


evalOperation : Operator -> Eval ()
evalOperation op =
    popOperand
        |> Eval.andThen
            (\right ->
                popOperand
                    |> Eval.andThen
                        (\left ->
                            evalBinOp op left right
                        )
            )


evalBinOp : Operator -> Rational -> Rational -> Eval ()
evalBinOp op a b =
    pushOperand <|
        case op of
            Add ->
                Rational.add a b

            Sub ->
                Rational.sub a b

            Mul ->
                Rational.mul a b

            Div ->
                Rational.div a b


pushOperand : Rational -> Eval ()
pushOperand q =
    Eval.modifyState (\state -> { state | operands = Stack.push q state.operands })


popOperand : Eval Rational
popOperand =
    Eval.actOnState
        (\state ->
            case Stack.pop state.operands of
                Just ( q, operands ) ->
                    ( Eval.succeed q, { state | operands = operands } )

                Nothing ->
                    ( Eval.fail SyntaxError, state )
        )


pushOperator : Operator -> Eval ()
pushOperator op =
    Eval.modifyState (\state -> { state | operators = Stack.push op state.operators })


popOperator : (Operator -> Eval ()) -> Eval () -> Eval ()
popOperator onOperator onEmpty =
    Eval.actOnState
        (\state ->
            case Stack.pop state.operators of
                Just ( op, operators ) ->
                    ( onOperator op, { state | operators = operators } )

                Nothing ->
                    ( onEmpty, state )
        )


precedence : Operator -> Int
precedence op =
    case op of
        Add ->
            1

        Sub ->
            1

        Mul ->
            2

        Div ->
            2
