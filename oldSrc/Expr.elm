module Expr exposing (Expr(..), eval, toString)


import Operator exposing (Operator(..))
import Rational exposing (Rational)
import Stack exposing (Stack)


type Expr
  = Const Rational
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr


eval : Expr -> Rational
eval expr =
  evalTokens (tokenize expr) Stack.new Stack.new


type Token
  = Number Rational
  | Symbol Operator


tokenize : Expr -> List Token
tokenize expr =
  case expr of
    Const n ->
      [Number n]

    Add a b ->
      tokenize a ++ [Symbol Plus] ++ tokenize b

    Sub a b ->
      tokenize a ++ [Symbol Minus] ++ tokenize b

    Mul a b ->
      tokenize a ++ [Symbol Times] ++ tokenize b

    Div a b ->
      tokenize a ++ [Symbol Division] ++ tokenize b


evalTokens : List Token -> Stack Operator -> Stack Rational -> Rational
evalTokens tokens operators operands =
  case tokens of
    [] ->
      evalOperators operators operands

    (token :: rest) ->
      case token of
        Number n ->
          evalTokens rest operators (Stack.push n operands)

        Symbol op ->
          let
            (newOperators, newOperands) =
              evalDominantOperators operators operands op
          in
            evalTokens rest newOperators newOperands


evalDominantOperators : Stack Operator -> Stack Rational -> Operator -> (Stack Operator, Stack Rational)
evalDominantOperators operators operands op =
  case Stack.pop operators of
    Nothing ->
      (Stack.push op operators, operands)

    Just (topOp, newOperators) ->
      if precedence op <= precedence topOp then
        case evalOperands topOp operands of
          Nothing ->
            (operators, operands)

          Just newOperands ->
            evalDominantOperators newOperators newOperands op
      else
        (Stack.push op operators, operands)


evalOperators : Stack Operator -> Stack Rational -> Rational
evalOperators operators operands =
  case Stack.pop operators of
    Nothing ->
      case Stack.pop operands of
        Nothing ->
          Rational.zero

        Just (result, _) ->
          result

    Just (op, newOperators) ->
      case evalOperands op operands of
        Nothing ->
          Rational.zero

        Just newOperands ->
          evalOperators newOperators newOperands


evalOperands : Operator -> Stack Rational -> Maybe (Stack Rational)
evalOperands op operands =
  case Stack.pop operands of
    Nothing ->
      Nothing

    Just (right, operandsMinus1) ->
      case Stack.pop operandsMinus1 of
        Nothing ->
          Nothing

        Just (left, operandsMinus2) ->
          Just <|
            Stack.push (evalBinop op left right) operandsMinus2


evalBinop : Operator -> Rational -> Rational -> Rational
evalBinop op =
  case op of
    Plus ->
      Rational.add

    Minus ->
      Rational.sub

    Times ->
      Rational.mul

    Division ->
      Rational.div


precedence : Operator -> Int
precedence op =
  case op of
    Plus ->
      1

    Minus ->
      1

    Times ->
      2

    Division ->
      2


toString : Expr -> String
toString expr =
  case expr of
    Const n ->
      Rational.toDecimalString n

    Add a b ->
      toString a ++ "+" ++ toString b

    Sub a b ->
      toString a ++ "-" ++ toString b

    Mul a b ->
      toString a ++ "*" ++ toString b

    Div a b ->
      toString a ++ "/" ++ toString b
