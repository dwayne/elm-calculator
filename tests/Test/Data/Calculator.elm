module Test.Data.Calculator exposing (suite)

import Data.Calculator as Calculator
import Data.Digit as Digit exposing (Digit(..))
import Data.Key as Key exposing (Key(..))
import Data.Operator as Operator exposing (Operator(..))
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Calculator"
        [ pressSuite
        , decimalInputSuite
        , operatorPrecedenceSuite
        , negativeDivisionSuite
        ]


pressSuite : Test
pressSuite =
    describe "press"
        [ describe "when nothing has been entered" <|
            let
                calculator =
                    Calculator.new
            in
            [ test "pressing AC" <|
                \_ ->
                    calculator
                        |> Calculator.press AC
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "", line2 = "0" }
            , test "pressing a digit" <|
                \_ ->
                    calculator
                        |> Calculator.press (Digit One)
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "1", line2 = "1" }
            , test "pressing an operator" <|
                \_ ->
                    calculator
                        |> Calculator.press (Operator Add)
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "", line2 = "0" }
            , test "pressing dot" <|
                \_ ->
                    calculator
                        |> Calculator.press Dot
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "0", line2 = "0." }
            , test "pressing =" <|
                \_ ->
                    calculator
                        |> Calculator.press Equal
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "", line2 = "0" }
            ]
        , describe "when a number has been entered" <|
            let
                calculator =
                    Calculator.new
                        |> Calculator.press (Digit One)
                        |> Calculator.press (Digit Two)
            in
            [ test "pressing AC" <|
                \_ ->
                    calculator
                        |> Calculator.press AC
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "", line2 = "0" }
            , test "pressing a digit" <|
                \_ ->
                    calculator
                        |> Calculator.press (Digit Three)
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "123", line2 = "123" }
            , test "pressing an operator" <|
                \_ ->
                    calculator
                        |> Calculator.press (Operator Add)
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "12+", line2 = "+" }
            , test "pressing dot" <|
                \_ ->
                    calculator
                        |> Calculator.press Dot
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "12", line2 = "12." }
            , test "pressing =" <|
                \_ ->
                    calculator
                        |> Calculator.press Equal
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "12=12", line2 = "12" }
            ]
        , describe "when a number and operator has been entered" <|
            let
                calculator =
                    Calculator.new
                        |> Calculator.press (Digit One)
                        |> Calculator.press (Digit Two)
                        |> Calculator.press (Operator Add)
            in
            [ test "pressing AC" <|
                \_ ->
                    calculator
                        |> Calculator.press AC
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "", line2 = "0" }
            , test "pressing a digit" <|
                \_ ->
                    calculator
                        |> Calculator.press (Digit Three)
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "12+3", line2 = "3" }
            , test "pressing an operator" <|
                \_ ->
                    calculator
                        |> Calculator.press (Operator Sub)
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "12-", line2 = "-" }
            , test "pressing dot" <|
                \_ ->
                    calculator
                        |> Calculator.press Dot
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "12+0", line2 = "0." }
            , test "pressing =" <|
                \_ ->
                    calculator
                        |> Calculator.press Equal
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "12=12", line2 = "12" }
            ]
        , describe "when a complete expression has been entered" <|
            let
                calculator =
                    Calculator.new
                        |> Calculator.press (Digit One)
                        |> Calculator.press (Digit Two)
                        |> Calculator.press (Operator Add)
                        |> Calculator.press (Digit Three)
                        |> Calculator.press (Operator Sub)
                        |> Calculator.press (Digit Four)
            in
            [ test "pressing AC" <|
                \_ ->
                    calculator
                        |> Calculator.press AC
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "", line2 = "0" }
            , test "pressing a digit" <|
                \_ ->
                    calculator
                        |> Calculator.press (Digit Five)
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "12+3-45", line2 = "45" }
            , test "pressing an operator" <|
                \_ ->
                    calculator
                        |> Calculator.press (Operator Add)
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "12+3-4+", line2 = "+" }
            , test "pressing dot" <|
                \_ ->
                    calculator
                        |> Calculator.press Dot
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "12+3-4", line2 = "4." }
            , test "pressing =" <|
                \_ ->
                    calculator
                        |> Calculator.press Equal
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "12+3-4=11", line2 = "11" }
            ]
        , describe "when an answer is given" <|
            let
                calculator =
                    Calculator.new
                        |> Calculator.press (Digit One)
                        |> Calculator.press (Digit Two)
                        |> Calculator.press (Operator Add)
                        |> Calculator.press (Digit Three)
                        |> Calculator.press (Operator Sub)
                        |> Calculator.press (Digit Four)
                        |> Calculator.press Equal
            in
            [ test "pressing AC" <|
                \_ ->
                    calculator
                        |> Calculator.press AC
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "", line2 = "0" }
            , test "pressing a digit" <|
                \_ ->
                    calculator
                        |> Calculator.press (Digit Nine)
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "9", line2 = "9" }
            , test "pressing an operator" <|
                \_ ->
                    calculator
                        |> Calculator.press (Operator Add)
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "11+", line2 = "+" }
            , test "pressing dot" <|
                \_ ->
                    calculator
                        |> Calculator.press Dot
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "0", line2 = "0." }
            , test "pressing =" <|
                \_ ->
                    calculator
                        |> Calculator.press Equal
                        |> Calculator.toOutput
                        |> Expect.equal { line1 = "12+3-4=11", line2 = "11" }
            ]
        ]


decimalInputSuite : Test
decimalInputSuite =
    describe "decimal line1" <|
        [ test "leading zeros are preserved" <|
            \_ ->
                let
                    calculator =
                        Calculator.new
                            |> Calculator.press Dot
                            |> Calculator.press (Digit Zero)
                            |> Calculator.press (Digit Zero)
                            |> Calculator.press (Digit Zero)
                in
                calculator
                    |> Calculator.toOutput
                    |> Expect.equal { line1 = "0", line2 = "0.000" }
        , test "trailing zeros are preserved" <|
            \_ ->
                let
                    calculator =
                        Calculator.new
                            |> Calculator.press Dot
                            |> Calculator.press (Digit Zero)
                            |> Calculator.press (Digit Zero)
                            |> Calculator.press (Digit Zero)
                            |> Calculator.press (Digit Five)
                            |> Calculator.press (Digit Zero)
                            |> Calculator.press (Digit Zero)
                            |> Calculator.press (Digit Zero)
                in
                calculator
                    |> Calculator.toOutput
                    |> Expect.equal { line1 = "0.0005", line2 = "0.0005000" }
        ]


operatorPrecedenceSuite : Test
operatorPrecedenceSuite =
    describe "operator precedence" <|
        [ test "multiplication is done before addition" <|
            \_ ->
                let
                    calculator =
                        Calculator.new
                            |> Calculator.press (Digit One)
                            |> Calculator.press (Operator Add)
                            |> Calculator.press (Digit Two)
                            |> Calculator.press (Operator Mul)
                            |> Calculator.press (Digit Three)
                            |> Calculator.press Equal
                in
                calculator
                    |> Calculator.toOutput
                    |> Expect.equal { line1 = "1+2×3=7", line2 = "7" }
        ]


negativeDivisionSuite : Test
negativeDivisionSuite =
    describe "negative division" <|
        [ test "negative divided by positive" <|
            \_ ->
                let
                    calculator =
                        Calculator.new
                            |> Calculator.press (Digit One)
                            |> Calculator.press (Operator Sub)
                            |> Calculator.press (Digit Two)
                            |> Calculator.press Equal
                            |> Calculator.press (Operator Div)
                            |> Calculator.press (Digit Three)
                            |> Calculator.press Equal
                in
                calculator
                    |> Calculator.toOutput
                    |> Expect.equal { line1 = "-1÷3=-0.(3)", line2 = "-0.(3)" }
        ]
