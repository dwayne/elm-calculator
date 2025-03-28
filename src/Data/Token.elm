module Data.Token exposing (Token(..), toString)

import Data.Operator as Operator exposing (Operator)
import Lib.Rational as Rational exposing (Rational)


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
