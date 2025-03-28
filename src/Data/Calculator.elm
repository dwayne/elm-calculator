module Data.Calculator exposing
    ( Calculator
    , Output
    , new
    , press
    , toOutput
    )

import Data.Digit as Digit
import Data.Evaluator as E
import Data.Key exposing (Key(..))
import Data.Operator as Operator exposing (Operator)
import Data.Token as Token exposing (Token)
import Lib.Rational as Rational exposing (Rational)


type Calculator
    = Calculator State


type State
    = Start
    | Left Decimal
    | Partial (List Token) Operator
    | Right (List Token) Operator Decimal
    | Answer (List Token) E.Answer


type Decimal
    = Whole Int
    | Fractional Int Int Int


new : Calculator
new =
    Calculator Start


press : Key -> Calculator -> Calculator
press key (Calculator state) =
    Calculator <| pressHelper key state


pressHelper : Key -> State -> State
pressHelper key state =
    case state of
        Start ->
            case key of
                AC ->
                    Start

                Digit digit ->
                    Left <| Whole <| Digit.toInt digit

                Operator _ ->
                    Start

                Dot ->
                    Left <| Fractional 0 0 1

                Equal ->
                    Start

        Left n ->
            case key of
                AC ->
                    Start

                Digit digit ->
                    let
                        d =
                            Digit.toInt digit
                    in
                    case n of
                        Whole w ->
                            Left <| Whole <| w * 10 + d

                        Fractional w f p ->
                            Left <| Fractional w (f * 10 + d) (p * 10)

                Operator op ->
                    Partial [ Token.Number <| toRational n ] op

                Dot ->
                    case n of
                        Whole w ->
                            Left <| Fractional w 0 1

                        Fractional _ _ _ ->
                            Left n

                Equal ->
                    let
                        r =
                            toRational n
                    in
                    Answer [ Token.Number r ] <| Ok r

        Partial tokens op ->
            case key of
                AC ->
                    Start

                Digit digit ->
                    Right tokens op <| Whole <| Digit.toInt digit

                Operator newOp ->
                    Partial tokens newOp

                Dot ->
                    Right tokens op <| Fractional 0 0 1

                Equal ->
                    Answer tokens <| eval tokens

        Right tokens op n ->
            case key of
                AC ->
                    Start

                Digit digit ->
                    let
                        d =
                            Digit.toInt digit
                    in
                    case n of
                        Whole w ->
                            Right tokens op <| Whole <| w * 10 + d

                        Fractional w f p ->
                            Right tokens op <| Fractional w (f * 10 + d) (p * 10)

                Operator newOp ->
                    let
                        newTokens =
                            Token.Number (toRational n) :: Token.Operator op :: tokens
                    in
                    Partial newTokens newOp

                Dot ->
                    case n of
                        Whole w ->
                            Right tokens op <| Fractional w 0 1

                        Fractional _ _ _ ->
                            Right tokens op n

                Equal ->
                    let
                        newTokens =
                            Token.Number (toRational n) :: Token.Operator op :: tokens
                    in
                    Answer newTokens <| eval newTokens

        Answer _ answer ->
            case key of
                AC ->
                    Start

                Digit digit ->
                    Left <| Whole <| Digit.toInt digit

                Operator op ->
                    case answer of
                        Ok r ->
                            Partial [ Token.Number r ] op

                        Err _ ->
                            state

                Dot ->
                    Left <| Fractional 0 0 1

                Equal ->
                    state


eval : List Token -> E.Answer
eval =
    E.eval << List.reverse


type alias Output =
    { line1 : String
    , line2 : String
    }


toOutput : Calculator -> Output
toOutput (Calculator state) =
    case state of
        Start ->
            Output "" "0"

        Left n ->
            let
                line1 =
                    Rational.toDecimalString <| toRational n

                line2 =
                    toPaddedDecimalString n
            in
            Output line1 line2

        Partial tokens op ->
            let
                line1 =
                    toExpr tokens ++ line2

                line2 =
                    Operator.toString op
            in
            Output line1 line2

        Right tokens op n ->
            let
                line1 =
                    toExpr tokens ++ Operator.toString op ++ right

                right =
                    Rational.toDecimalString <| toRational n

                line2 =
                    toPaddedDecimalString n
            in
            Output line1 line2

        Answer tokens answer ->
            let
                line1 =
                    toExpr tokens ++ "=" ++ line2

                line2 =
                    case answer of
                        Ok r ->
                            Rational.toDecimalString r

                        Err _ ->
                            "TODO: Display an appropriate error message."
            in
            Output line1 line2


toPaddedDecimalString : Decimal -> String
toPaddedDecimalString n =
    case n of
        Whole w ->
            String.fromInt w

        Fractional w f p ->
            String.concat
                [ String.fromInt w
                , "."
                , if f == 0 && p == 1 then
                    ""

                  else
                    String.padLeft
                        (String.length (String.fromInt p) - 1)
                        '0'
                        (String.fromInt f)
                ]


toExpr : List Token -> String
toExpr =
    String.concat << List.map Token.toString << List.reverse


toRational : Decimal -> Rational
toRational n =
    case n of
        Whole w ->
            Rational.fromInt w

        Fractional w f p ->
            Maybe.map2 Rational.add (Rational.new w 1) (Rational.new f p)
                |> Maybe.withDefault Rational.zero
