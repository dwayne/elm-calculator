module Test.Expr exposing (suite)


import Expect
import Test exposing (..)

import Expr exposing (Expr(..), eval)


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
          Expr.eval (Const 1)
            |> Expect.equal 1
    , test "1+2" <|
        \_ ->
          Expr.eval (Add (Const 1) (Const 2))
            |> Expect.equal 3
    , test "6-1" <|
        \_ ->
          Expr.eval (Sub (Const 6) (Const 1))
            |> Expect.equal 5
    , test "2*5" <|
        \_ ->
          Expr.eval (Mul (Const 2) (Const 5))
            |> Expect.equal 10
    ]


precedenceSuite : Test
precedenceSuite =
  describe "operator precedence" <|
    [ test "2+3-4" <|
        \_ ->
          Expr.eval (Add (Const 2) (Sub (Const 3) (Const 4)))
            |> Expect.equal 1
    , test "2-3+4" <|
        \_ ->
          Expr.eval (Sub (Const 2) (Add (Const 3) (Const 4)))
            |> Expect.equal 3
    , test "1+2*3" <|
        \_ ->
          Expr.eval (Add (Const 1) (Mul (Const 2) (Const 3)))
            |> Expect.equal 7
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
                          (Const 1)
                          (Const 2))
                        (Const 5))
                      (Const 8))
                    (Const 6))
                  (Const 10))
                (Const 3)
          in
            Expr.eval expr
              |> Expect.equal -61
    ]
