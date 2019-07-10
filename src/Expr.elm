module Expr exposing (Expr(..), eval, toString)


import Operator exposing (Operator(..))
import Stack exposing (Stack)


type Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr


eval : Expr -> Int
eval expr =
  evalTokens (tokenize expr) Stack.new Stack.new


type Token
  = Number Int
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


evalTokens : List Token -> Stack Operator -> Stack Int -> Int
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


evalDominantOperators : Stack Operator -> Stack Int -> Operator -> (Stack Operator, Stack Int)
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


evalOperators : Stack Operator -> Stack Int -> Int
evalOperators operators operands =
  case Stack.pop operators of
    Nothing ->
      case Stack.pop operands of
        Nothing ->
          0

        Just (result, _) ->
          result

    Just (op, newOperators) ->
      case evalOperands op operands of
        Nothing ->
          0

        Just newOperands ->
          evalOperators newOperators newOperands


evalOperands : Operator -> Stack Int -> Maybe (Stack Int)
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


evalBinop : Operator -> Int -> Int -> Int
evalBinop op a b =
  case op of
    Plus ->
      a + b

    Minus ->
      a - b

    Times ->
      a * b

    Division ->
      a // b


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
      String.fromInt n

    Add a b ->
      toString a ++ "+" ++ toString b

    Sub a b ->
      toString a ++ "-" ++ toString b

    Mul a b ->
      toString a ++ "*" ++ toString b

    Div a b ->
      toString a ++ "/" ++ toString b
