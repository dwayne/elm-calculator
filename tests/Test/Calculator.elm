module Test.Calculator exposing (suite)


import Expect
import Test exposing (..)

import Calculator exposing (Key(..))
import Operator exposing (Operator(..))


suite : Test
suite =
  describe "Calculator"
    [ processSuite
    , decimalInputSuite
    , operatorPrecedenceSuite
    , negativeDivisionSuite
    ]


processSuite : Test
processSuite =
  describe "process"
    [ describe "when nothing has been entered" <|
        let
          calculator =
            Calculator.new
        in
          [ test "pressing AC" <|
              \_ ->
                calculator
                  |> Calculator.process AC
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "", output = "0" }
          , test "pressing a digit" <|
              \_ ->
                calculator
                  |> Calculator.process (Digit 1)
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "1", output = "1" }
          , test "pressing an operator" <|
              \_ ->
                calculator
                  |> Calculator.process (Operator Plus)
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "", output = "0" }
          , test "pressing dot" <|
              \_ ->
                calculator
                  |> Calculator.process Dot
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "0", output = "0." }
          , test "pressing =" <|
              \_ ->
                calculator
                  |> Calculator.process Equal
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "", output = "0" }
          ]
    , describe "when a number has been entered" <|
        let
          calculator =
            Calculator.new
              |> Calculator.process (Digit 1)
              |> Calculator.process (Digit 2)
        in
          [ test "pressing AC" <|
              \_ ->
                calculator
                  |> Calculator.process AC
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "", output = "0" }
          , test "pressing a digit" <|
              \_ ->
                calculator
                  |> Calculator.process (Digit 3)
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "123", output = "123" }
          , test "pressing an operator" <|
              \_ ->
                calculator
                  |> Calculator.process (Operator Plus)
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "12+", output = "+" }
          , test "pressing dot" <|
              \_ ->
                calculator
                  |> Calculator.process Dot
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "12", output = "12." }
          , test "pressing =" <|
              \_ ->
                calculator
                  |> Calculator.process Equal
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "12=12", output = "12" }
          ]
    , describe "when a number and operator has been entered" <|
        let
          calculator =
            Calculator.new
              |> Calculator.process (Digit 1)
              |> Calculator.process (Digit 2)
              |> Calculator.process (Operator Plus)
        in
          [ test "pressing AC" <|
              \_ ->
                calculator
                  |> Calculator.process AC
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "", output = "0" }
          , test "pressing a digit" <|
              \_ ->
                calculator
                  |> Calculator.process (Digit 3)
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "12+3", output = "3" }
          , test "pressing an operator" <|
              \_ ->
                calculator
                  |> Calculator.process (Operator Minus)
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "12-", output = "-" }
          , test "pressing dot" <|
              \_ ->
                calculator
                  |> Calculator.process Dot
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "12+0", output = "0." }
          , test "pressing =" <|
              \_ ->
                calculator
                  |> Calculator.process Equal
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "12=12", output = "12" }
          ]
    , describe "when a complete expression has been entered" <|
        let
          calculator =
            Calculator.new
              |> Calculator.process (Digit 1)
              |> Calculator.process (Digit 2)
              |> Calculator.process (Operator Plus)
              |> Calculator.process (Digit 3)
              |> Calculator.process (Operator Minus)
              |> Calculator.process (Digit 4)
        in
          [ test "pressing AC" <|
              \_ ->
                calculator
                  |> Calculator.process AC
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "", output = "0" }
          , test "pressing a digit" <|
              \_ ->
                calculator
                  |> Calculator.process (Digit 5)
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "12+3-45", output = "45" }
          , test "pressing an operator" <|
              \_ ->
                calculator
                  |> Calculator.process (Operator Plus)
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "12+3-4+", output = "+" }
          , test "pressing dot" <|
              \_ ->
                calculator
                  |> Calculator.process Dot
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "12+3-4", output = "4." }
          , test "pressing =" <|
              \_ ->
                calculator
                  |> Calculator.process Equal
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "12+3-4=11", output = "11" }
          ]
    , describe "when an answer is given" <|
        let
          calculator =
            Calculator.new
              |> Calculator.process (Digit 1)
              |> Calculator.process (Digit 2)
              |> Calculator.process (Operator Plus)
              |> Calculator.process (Digit 3)
              |> Calculator.process (Operator Minus)
              |> Calculator.process (Digit 4)
              |> Calculator.process Equal
        in
          [ test "pressing AC" <|
              \_ ->
                calculator
                  |> Calculator.process AC
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "", output = "0" }
          , test "pressing a digit" <|
              \_ ->
                calculator
                  |> Calculator.process (Digit 9)
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "9", output = "9" }
          , test "pressing an operator" <|
              \_ ->
                calculator
                  |> Calculator.process (Operator Plus)
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "11+", output = "+" }
          , test "pressing dot" <|
              \_ ->
                calculator
                  |> Calculator.process Dot
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "0", output = "0." }
          , test "pressing =" <|
              \_ ->
                calculator
                  |> Calculator.process Equal
                  |> Calculator.toDisplay
                  |> Expect.equal { expr = "12+3-4=11", output = "11" }
          ]
    ]


decimalInputSuite : Test
decimalInputSuite =
  describe "decimal input" <|
    [ test "leading zeros are preserved" <|
        \_ ->
          let
            calculator =
              Calculator.new
                |> Calculator.process Dot
                |> Calculator.process (Digit 0)
                |> Calculator.process (Digit 0)
                |> Calculator.process (Digit 0)
          in
            calculator
              |> Calculator.toDisplay
              |> Expect.equal { expr = "0", output = "0.000" }
    , test "trailing zeros are preserved" <|
        \_ ->
          let
            calculator =
              Calculator.new
                |> Calculator.process Dot
                |> Calculator.process (Digit 0)
                |> Calculator.process (Digit 0)
                |> Calculator.process (Digit 0)
                |> Calculator.process (Digit 5)
                |> Calculator.process (Digit 0)
                |> Calculator.process (Digit 0)
                |> Calculator.process (Digit 0)
          in
            calculator
              |> Calculator.toDisplay
              |> Expect.equal { expr = "0.0005", output = "0.0005000" }
    ]


operatorPrecedenceSuite : Test
operatorPrecedenceSuite =
  describe "operator precedence" <|
    [ test "multiplication is done before addition" <|
        \_ ->
          let
            calculator =
              Calculator.new
                |> Calculator.process (Digit 1)
                |> Calculator.process (Operator Plus)
                |> Calculator.process (Digit 2)
                |> Calculator.process (Operator Times)
                |> Calculator.process (Digit 3)
                |> Calculator.process Equal
          in
            calculator
              |> Calculator.toDisplay
              |> Expect.equal { expr = "1+2*3=7", output = "7" }
    ]


negativeDivisionSuite : Test
negativeDivisionSuite =
  describe "negative division" <|
    [ test "negative divided by positive" <|
        \_ ->
          let
            calculator =
              Calculator.new
                |> Calculator.process (Digit 1)
                |> Calculator.process (Operator Minus)
                |> Calculator.process (Digit 2)
                |> Calculator.process Equal
                |> Calculator.process (Operator Division)
                |> Calculator.process (Digit 3)
                |> Calculator.process Equal
          in
            calculator
              |> Calculator.toDisplay
              |> Expect.equal { expr = "-1/3=-0.(3)", output = "-0.(3)" }
    ]
