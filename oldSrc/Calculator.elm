module Calculator exposing
  ( Calculator, Key(..)
  , new, process

  , Display
  , toDisplay
  )


import Expr exposing (Expr(..))
import Operator exposing (Operator(..))
import Rational exposing (Rational)


type Calculator
  = Start
  | Left Number
  | Partial Operator Expr
  | Right Number Operator Expr
  | Answer Rational Expr


type Number
  = Whole Int
  | Decimal Int Int Int


type Key
  = AC
  | Digit Int
  | Operator Operator
  | Dot
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
          Left (Whole d)

        Operator _ ->
          Start

        Dot ->
          Left (Decimal 0 0 1)

        Equal ->
          Start

    Left n ->
      case key of
        AC ->
          Start

        Digit d ->
          case n of
            Whole w ->
              Left (Whole (w * 10 + d))

            Decimal w f p ->
              Left (Decimal w (f * 10 + d) (p * 10))

        Operator op ->
          Partial op (Const (rational n))

        Dot ->
          case n of
            Whole w ->
              Left (Decimal w 0 1)

            Decimal w f p ->
              Left (Decimal w f p)

        Equal ->
          let
            r =
              rational n
          in
            Answer r (Const r)

    Partial op left ->
      case key of
        AC ->
          Start

        Digit d ->
          Right (Whole d) op left

        Operator newOp ->
          Partial newOp left

        Dot ->
          Right (Decimal 0 0 1) op left

        Equal ->
          Answer (Expr.eval left) left

    Right n op left ->
      case key of
        AC ->
          Start

        Digit d ->
          case n of
            Whole w ->
              Right (Whole (w * 10 + d)) op left

            Decimal w f p ->
              Right (Decimal w (f * 10 + d) (p * 10)) op left

        Operator newOp ->
          Partial newOp (operatorToExpr op left (Const (rational n)))

        Dot ->
          case n of
            Whole w ->
              Right (Decimal w 0 1) op left

            Decimal w f p ->
              Right (Decimal w f p) op left

        Equal ->
          let
            expr =
              operatorToExpr op left (Const (rational n))
          in
            Answer (Expr.eval expr) expr

    Answer n expr ->
      case key of
        AC ->
          Start

        Digit d ->
          Left (Whole d)

        Operator op ->
          Partial op (Const n)

        Dot ->
          Left (Decimal 0 0 1)

        Equal ->
          Answer n expr


rational : Number -> Rational
rational n =
  case n of
    Whole w ->
      Rational.fromInt w

    Decimal w f p ->
      Maybe.map2 Rational.add (Rational.new w 1) (Rational.new f p)
        |> Maybe.withDefault Rational.zero


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
        expr =
          Rational.toDecimalString (rational n)

        output =
          toPaddedDecimalString n
      in
        Display expr output

    Partial op left ->
      let
        opAsString =
          Operator.toString op
      in
        Display (Expr.toString left ++ opAsString) opAsString

    Right n op left ->
      let
        expr =
          Rational.toDecimalString (rational n)

        output =
          toPaddedDecimalString n
      in
        Display (Expr.toString left ++ Operator.toString op ++ expr) output

    Answer n expr ->
      let
        s =
          Rational.toDecimalString n
      in
        Display (Expr.toString expr ++ "=" ++ s) s


toPaddedDecimalString : Number -> String
toPaddedDecimalString n =
  case n of
    Whole w ->
      String.fromInt w

    Decimal w f p ->
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
