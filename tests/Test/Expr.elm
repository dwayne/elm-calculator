module Test.Expr exposing (suite)


import Expect
import Test exposing (..)

import Expr exposing (Expr(..), eval)
import Rational exposing (Rational)


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
          Expr.eval (const 1)
            |> Expect.equal (int 1)
    , test "1+2" <|
        \_ ->
          Expr.eval (Add (const 1) (const 2))
            |> Expect.equal (int 3)
    , test "6-1" <|
        \_ ->
          Expr.eval (Sub (const 6) (const 1))
            |> Expect.equal (int 5)
    , test "2*5" <|
        \_ ->
          Expr.eval (Mul (const 2) (const 5))
            |> Expect.equal (int 10)
    , test "10/2" <|
        \_ ->
          Expr.eval (Div (const 10) (const 2))
            |> Expect.equal (int 5)
    , test "5/2" <|
        \_ ->
          Expr.eval (Div (const 5) (const 2))
            |> Expect.equal (rational 5 2)
    , test "1/0" <|
        \_ ->
          Expr.eval (Div (const 1) (const 0))
            |> Expect.equal (int 0)
    ]


precedenceSuite : Test
precedenceSuite =
  describe "operator precedence" <|
    [ test "2+3-4" <|
        \_ ->
          Expr.eval (Add (const 2) (Sub (const 3) (const 4)))
            |> Expect.equal (int 1)
    , test "2-3+4" <|
        \_ ->
          Expr.eval (Sub (const 2) (Add (const 3) (const 4)))
            |> Expect.equal (int 3)
    , test "1+2*3" <|
        \_ ->
          Expr.eval (Add (const 1) (Mul (const 2) (const 3)))
            |> Expect.equal (int 7)
    , test "1+3/3" <|
        \_ ->
          Expr.eval (Div (Add (const 1) (const 3)) (const 3))
            |> Expect.equal (int 2)
    , test "1+2-5*8+6-10*3" <|
        \_ ->
          let
            expr =
              Mul
                (Sub
                  (Add
                    (Mul
                      (Sub
                        (Add
                          (const 1)
                          (const 2))
                        (const 5))
                      (const 8))
                    (const 6))
                  (const 10))
                (const 3)
          in
            Expr.eval expr
              |> Expect.equal (int -61)
    ]


-- HELPERS


const : Int -> Expr
const =
  Const << Rational.fromInt


int : Int -> Rational
int =
  Rational.fromInt


rational : Int -> Int -> Rational
rational n d =
  Maybe.withDefault (Rational.zero) (Rational.new n d)
