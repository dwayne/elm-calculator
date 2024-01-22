module Token exposing (Token(..), toString)

import Operator exposing (Operator)
import Rational exposing (Rational)


type Token
    = Number Rational
    | Operator Operator


toString : Token -> String
toString token =
    case token of
        Number r ->
            Rational.toDecimalString r

        Operator op ->
            Operator.toString op
