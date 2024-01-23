module Key exposing (Key(..), toString)

import Digit exposing (Digit)
import Operator exposing (Operator)


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
