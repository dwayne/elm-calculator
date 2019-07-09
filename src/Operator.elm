module Operator exposing (Operator(..), toString)


type Operator
  = Plus
  | Minus
  | Times


toString : Operator -> String
toString op =
  case op of
    Plus ->
      "+"

    Minus ->
      "-"

    Times ->
      "*"
