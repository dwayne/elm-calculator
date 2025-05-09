module Data.Evaluator exposing (Answer, Error(..), eval)

import Data.Operator exposing (Operator(..))
import Data.Token exposing (Token(..))
import Lib.Rational as Rational exposing (Rational)
import Lib.Stack as Stack exposing (Stack)


type alias Answer =
    Result Error Rational


type Error
    = SyntaxError


type alias State =
    { operands : Stack Rational
    , operators : Stack Operator
    }


eval : List Token -> Answer
eval tokens =
    --
    -- Evaluates infix expressions using Dijkstra's shunting yard algorithm.
    --
    let
        state =
            { operands = Stack.new
            , operators = Stack.new
            }
    in
    evalTokens tokens state
        |> Result.andThen
            (\{ operands, operators } ->
                case ( Stack.pop operands, Stack.isEmpty operators ) of
                    ( Just ( value, newOperands ), True ) ->
                        if Stack.isEmpty newOperands then
                            Ok value

                        else
                            Err SyntaxError

                    _ ->
                        Err SyntaxError
            )


evalTokens : List Token -> State -> Result Error State
evalTokens tokens state =
    case tokens of
        [] ->
            evalOperators state

        token :: restTokens ->
            evalToken token state
                |> Result.andThen (evalTokens restTokens)


evalToken : Token -> State -> Result Error State
evalToken token state =
    case token of
        Number n ->
            pushOperand n state

        Operator op ->
            evalDominantOperators op state


evalDominantOperators : Operator -> State -> Result Error State
evalDominantOperators op1 state0 =
    popOperator
        (\op2 state1 ->
            if precedence op2 >= precedence op1 then
                evalOperation op2 state1
                    |> Result.andThen (evalDominantOperators op1)

            else
                pushOperator op1 state0
        )
        (pushOperator op1)
        state0


evalOperators : State -> Result Error State
evalOperators =
    popOperator
        (\op -> evalOperation op >> Result.andThen evalOperators)
        Ok


evalOperation : Operator -> State -> Result Error State
evalOperation op state0 =
    popOperand state0
        |> Result.andThen
            (\( right, state1 ) ->
                popOperand state1
                    |> Result.andThen
                        (\( left, state2 ) ->
                            evalBinOp op left right state2
                        )
            )


evalBinOp : Operator -> Rational -> Rational -> State -> Result Error State
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


pushOperand : Rational -> State -> Result Error State
pushOperand q state =
    Ok { state | operands = Stack.push q state.operands }


popOperand : State -> Result Error ( Rational, State )
popOperand state =
    case Stack.pop state.operands of
        Just ( q, operands ) ->
            Ok ( q, { state | operands = operands } )

        Nothing ->
            Err SyntaxError


pushOperator : Operator -> State -> Result Error State
pushOperator op state =
    Ok { state | operators = Stack.push op state.operators }


popOperator : (Operator -> State -> Result Error State) -> (State -> Result Error State) -> State -> Result Error State
popOperator onOperator onEmpty state =
    case Stack.pop state.operators of
        Just ( op, operators ) ->
            onOperator op { state | operators = operators }

        Nothing ->
            onEmpty state


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
