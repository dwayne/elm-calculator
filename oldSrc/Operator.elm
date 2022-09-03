module Operator exposing (Operator(..), toString)


type Operator
  = Plus
  | Minus
  | Times
  | Division


toString : Operator -> String
toString op =
  case op of
    Plus ->
      "+"

    Minus ->
      "-"

    Times ->
      "*"

    Division ->
      "/"
