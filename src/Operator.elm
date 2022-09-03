module Operator exposing
  ( Operator(..)
  , toMathString, toString
  )


type Operator
  = Add
  | Sub
  | Mul
  | Div


toMathString : Operator -> String
toMathString operator =
  case operator of
    Add ->
      "+"

    Sub ->
      "-"

    Mul ->
      "×"

    Div ->
      "÷"


toString : Operator -> String
toString operator =
  case operator of
    Add ->
      "+"

    Sub ->
      "-"

    Mul ->
      "*"

    Div ->
      "/"
