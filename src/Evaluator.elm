module Evaluator exposing (Answer, Error(..), eval)

import Operator exposing (Operator(..))
import Rational exposing (Rational)
import Stack exposing (Stack)
import Token exposing (Token(..))


type alias Answer =
    Result Error Rational


type Error
    = SyntaxError


type alias State =
    { operators : Stack Operator
    , operands : Stack Rational
    }



--
-- eval is an implementation of Edsger Dijkstra's shunting yard algorithm.
--


eval : List Token -> Answer
eval tokens =
    let
        state =
            { operators = Stack.new
            , operands = Stack.new
            }
    in
    evalTokens tokens state
        |> Result.andThen
            (\{ operators, operands } ->
                case ( Stack.isEmpty operators, Stack.pop operands ) of
                    ( True, Just ( r, newOperands ) ) ->
                        if Stack.isEmpty newOperands then
                            Ok r

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
evalDominantOperators op state =
    if hasOperator state then
        popOperator state
            |> Result.andThen
                (\( topOp, newState ) ->
                    if precedence topOp >= precedence op then
                        evalOperation topOp newState
                            |> Result.andThen (evalDominantOperators op)

                    else
                        pushOperator op state
                )

    else
        pushOperator op state


evalOperators : State -> Result Error State
evalOperators state =
    if hasOperator state then
        popOperator state
            |> Result.andThen
                (\( op, newState ) ->
                    evalOperation op newState
                        |> Result.andThen evalOperators
                )

    else
        Ok state


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
evalBinOp op a b state =
    case op of
        Add ->
            pushOperand (Rational.add a b) state

        Sub ->
            pushOperand (Rational.sub a b) state

        Mul ->
            pushOperand (Rational.mul a b) state

        Div ->
            pushOperand (Rational.div a b) state


pushOperand : Rational -> State -> Result Error State
pushOperand r state =
    Ok { state | operands = Stack.push r state.operands }


popOperand : State -> Result Error ( Rational, State )
popOperand state =
    case Stack.pop state.operands of
        Nothing ->
            Err SyntaxError

        Just ( op, operands ) ->
            Ok ( op, { state | operands = operands } )


hasOperator : State -> Bool
hasOperator { operators } =
    not <| Stack.isEmpty operators


pushOperator : Operator -> State -> Result Error State
pushOperator op state =
    Ok { state | operators = Stack.push op state.operators }


popOperator : State -> Result Error ( Operator, State )
popOperator state =
    case Stack.pop state.operators of
        Nothing ->
            Err SyntaxError

        Just ( op, operators ) ->
            Ok ( op, { state | operators = operators } )


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
