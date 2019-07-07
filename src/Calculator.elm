module Calculator exposing
  ( Calculator, Key(..), Operator(..)
  , new, process

  , Display
  , toDisplay
  )


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


type Operator
  = Plus
  | Minus


type Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr


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
          Answer (eval left) left

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
            Answer (eval expr) expr

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


eval : Expr -> Int
eval expr =
  case expr of
    Const n ->
      n

    Add a b ->
      (eval a) + (eval b)

    Sub a b ->
      (eval a) - (eval b)


operatorToExpr : Operator -> Expr -> Expr -> Expr
operatorToExpr op =
  case op of
    Plus ->
      Add

    Minus ->
      Sub


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
          operatorToString op
      in
        Display (exprToString left ++ opAsString) opAsString

    Right n op left ->
      let
        s =
          String.fromInt n
      in
        Display (exprToString left ++ operatorToString op ++ s) s

    Answer n expr ->
      let
        s =
          String.fromInt n
      in
        Display (exprToString expr ++ "=" ++ s) s


exprToString : Expr -> String
exprToString expr =
  case expr of
    Const n ->
      String.fromInt n

    Add a b ->
      (exprToString a) ++ "+" ++ (exprToString b)

    Sub a b ->
      (exprToString a) ++ "-" ++ (exprToString b)


operatorToString : Operator -> String
operatorToString op =
  case op of
    Plus ->
      "+"

    Minus ->
      "-"
