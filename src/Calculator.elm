module Calculator exposing
  ( Calculator, Key(..)
  , new, process

  , Display
  , toDisplay
  )


import Expr exposing (Expr(..))
import Operator exposing (Operator(..))


type Calculator
  = Start
  | Left Int
  | Partial Operator Expr
  | Right Int Operator Expr
  | Answer Int Expr


type Key
  = AC
  | Digit Int
  | Operator Operator
  | Equal


new : Calculator
new =
  Start


process : Key -> Calculator -> Calculator
process key calculator =
  case calculator of
    Start ->
      case key of
        AC ->
          Start

        Digit d ->
          Left d

        Operator _ ->
          Start

        Equal ->
          Start

    Left n ->
      case key of
        AC ->
          Start

        Digit d ->
          Left (n * 10 + d)

        Operator op ->
          Partial op (Const n)

        Equal ->
          Answer n (Const n)

    Partial op left ->
      case key of
        AC ->
          Start

        Digit d ->
          Right d op left

        Operator newOp ->
          Partial newOp left

        Equal ->
          Answer (Expr.eval left) left

    Right n op left ->
      case key of
        AC ->
          Start

        Digit d ->
          Right (n * 10 + d) op left

        Operator newOp ->
          Partial newOp (operatorToExpr op left (Const n))

        Equal ->
          let
            expr =
              operatorToExpr op left (Const n)
          in
            Answer (Expr.eval expr) expr

    Answer n expr ->
      case key of
        AC ->
          Start

        Digit d ->
          Left d

        Operator op ->
          Partial op (Const n)

        Equal ->
          Answer n expr


operatorToExpr : Operator -> Expr -> Expr -> Expr
operatorToExpr op =
  case op of
    Plus ->
      Add

    Minus ->
      Sub

    Times ->
      Mul

    Division ->
      Div


type alias Display =
  { expr : String
  , output : String
  }


toDisplay : Calculator -> Display
toDisplay calculator =
  case calculator of
    Start ->
      Display "" "0"

    Left n ->
      let
        s =
          String.fromInt n
      in
        Display s s

    Partial op left ->
      let
        opAsString =
          Operator.toString op
      in
        Display (Expr.toString left ++ opAsString) opAsString

    Right n op left ->
      let
        s =
          String.fromInt n
      in
        Display (Expr.toString left ++ Operator.toString op ++ s) s

    Answer n expr ->
      let
        s =
          String.fromInt n
      in
        Display (Expr.toString expr ++ "=" ++ s) s
