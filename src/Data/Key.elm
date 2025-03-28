module Data.Key exposing (Key(..), toString)

import Data.Digit as Digit exposing (Digit)
import Data.Operator as Operator exposing (Operator)


type Key
    = AC
    | Dot
    | Equal
    | Digit Digit
    | Operator Operator


toString : Key -> String
toString key =
    case key of
        AC ->
            "AC"

        Dot ->
            "."

        Equal ->
            "="

        Digit digit ->
            Digit.toString digit

        Operator operator ->
            Operator.toString operator
