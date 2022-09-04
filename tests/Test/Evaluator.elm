module Test.Evaluator exposing (suite)


import Expect exposing (Expectation)
import Test exposing (..)

import Evaluator as E
import Operator exposing (Operator(..))
import Rational exposing (Rational)
import Token exposing (Token)


suite : Test
suite =
  describe "Expr"
    [ evalSuite ]


evalSuite : Test
evalSuite =
  describe "eval" <|
    [ simpleSuite
    , precedenceSuite
    ]


simpleSuite : Test
simpleSuite =
  describe "simple expressions" <|
    [ test "1" <|
        \_ ->
          expectEval
            [intT 1]
            (int 1)

    , test "1+2" <|
        \_ ->
          expectEval
            [intT 1, operatorT Add, intT 2]
            (int 3)
    , test "6-1" <|
        \_ ->
          expectEval
            [intT 6, operatorT Sub, intT 1]
            (int 5)
    , test "2*5" <|
        \_ ->
          expectEval
            [intT 2, operatorT Mul, intT 5]
            (int 10)
    , test "10/2" <|
        \_ ->
          expectEval
            [intT 10, operatorT Div, intT 2]
            (int 5)
    , test "5/2" <|
        \_ ->
          expectEval
            [intT 5, operatorT Div, intT 2]
            (rational 5 2)
    , test "1/0" <|
        \_ ->
          expectEval
            [intT 1, operatorT Div, intT 0]
            (int 0)
    ]


precedenceSuite : Test
precedenceSuite =
  describe "operator precedence" <|
    [ test "2+3-4" <|
        \_ ->
          expectEval
            [intT 2, operatorT Add, intT 3, operatorT Sub, intT 4]
            (int 1)
    , test "2-3+4" <|
        \_ ->
          expectEval
            [intT 2, operatorT Sub, intT 3, operatorT Add, intT 4]
            (int 3)
    , test "1+2*3" <|
        \_ ->
          expectEval
            [intT 1, operatorT Add, intT 2, operatorT Mul, intT 3]
            (int 7)
    , test "1+3/3" <|
        \_ ->
          expectEval
            [intT 1, operatorT Add, intT 3, operatorT Div, intT 3]
            (int 2)
    , test "1+2-5*8+6-10*3" <|
        \_ ->
          expectEval
            [ intT 1
            , operatorT Add
            , intT 2
            , operatorT Sub
            , intT 5
            , operatorT Mul
            , intT 8
            , operatorT Add
            , intT 6
            , operatorT Sub
            , intT 10
            , operatorT Mul
            , intT 3
            ]
            (int -61)
    ]


-- HELPERS


expectEval : List Token -> Rational -> Expectation
expectEval tokens expectedRational =
  case E.eval tokens of
    Ok actualRational ->
      Expect.equal expectedRational actualRational

    Err _ ->
      Expect.fail "eval failed"


int : Int -> Rational
int =
  Rational.fromInt


rational : Int -> Int -> Rational
rational n d =
  Rational.new n d
    |> Maybe.withDefault Rational.zero


intT : Int -> Token
intT =
  Token.Number << int


operatorT : Operator -> Token
operatorT =
  Token.Operator
