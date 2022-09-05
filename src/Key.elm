module Key exposing
  ( Key(..)
  , toString, toMathString

  , Options, Style(..)
  , view
  )


import Digit exposing (Digit)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Operator exposing (Operator)


type Key
  = AC
  | Dot
  | Equal
  | Digit Digit
  | Operator Operator


toString : Key -> String
toString key =
  case key of
    AC ->
      "AC"

    Dot ->
      "."

    Equal ->
      "="

    Digit digit ->
      Digit.toString digit

    Operator operator ->
      Operator.toString operator


toMathString : Key -> String
toMathString key =
  case key of
    AC ->
      "AC"

    Dot ->
      "."

    Equal ->
      "="

    Digit digit ->
      Digit.toString digit

    Operator operator ->
      Operator.toMathString operator


-- VIEW


type alias Options msg =
  { style : Style
  , onClick : Key -> msg
  }


type Style
  = Default
  | Primary
  | Secondary


view : Options msg -> Key -> H.Html msg
view { style, onClick } key =
  H.button
    [ HA.class "key"
    , HA.class <|
        case style of
          Default ->
            ""

          Primary ->
            "key--primary"

          Secondary ->
            "key--secondary"
    , HE.onClick <| onClick key
    ]
    [ H.text <| toMathString key ]
