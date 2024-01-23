module Operator exposing (Operator(..), toString)


type Operator
    = Add
    | Sub
    | Mul
    | Div


toString : Operator -> String
toString operator =
    case operator of
        Add ->
            "+"

        Sub ->
            "-"

        Mul ->
            "×"

        Div ->
            "÷"
