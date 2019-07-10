module Main exposing (main)


import Browser
import Html exposing (Html, a, button, div, footer, text)
import Html.Attributes exposing (class, disabled, href, target)
import Html.Events exposing (onClick)

import Calculator exposing (Calculator, Key(..))
import Operator exposing (Operator(..))


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }


-- MODEL


type alias Model =
  { calculator : Calculator
  }


init : Model
init =
  { calculator = Calculator.new
  }


-- UPDATE


type Msg
  = Clicked Key


update : Msg -> Model -> Model
update msg model =
  case msg of
    Clicked key ->
      { model | calculator = Calculator.process key model.calculator }


-- VIEW


view : Model -> Html Msg
view { calculator } =
  div [ class "inline-block" ]
    [ viewCalculator calculator
    , viewAttribution
    ]


viewCalculator : Calculator -> Html Msg
viewCalculator calculator =
  let
    display =
      Calculator.toDisplay calculator
  in
    div [ class "calculator" ]
      [ div [ class "calculator__expr" ]
          [ if String.isEmpty display.expr then
              text (String.fromChar nonBreakingSpace)
            else
              text display.expr
          ]
      , div [ class "calculator__output" ] [ text display.output ]
      , div [ class "calculator__buttons" ]
          [ button [ class "r0 c0 colspan2 bg-red", onClick (Clicked AC) ]
              [ text "AC" ]
          , button [ class "r0 c2", onClick (Clicked (Operator Division)) ]
              [ text "÷" ]
          , button [ class "r0 c3", onClick (Clicked (Operator Times)) ]
              [ text "×" ]
          , button [ class "r1 c0", onClick (Clicked (Digit 7)) ]
              [ text "7" ]
          , button [ class "r1 c1", onClick (Clicked (Digit 8)) ]
              [ text "8" ]
          , button [ class "r1 c2", onClick (Clicked (Digit 9)) ]
              [ text "9" ]
          , button [ class "r1 c3", onClick (Clicked (Operator Minus)) ]
              [ text "-" ]
          , button [ class "r2 c0", onClick (Clicked (Digit 4)) ]
              [ text "4" ]
          , button [ class "r2 c1", onClick (Clicked (Digit 5)) ]
              [ text "5" ]
          , button [ class "r2 c2", onClick (Clicked (Digit 6)) ]
              [ text "6" ]
          , button [ class "r2 c3", onClick (Clicked (Operator Plus)) ]
              [ text "+" ]
          , button [ class "r3 c0", onClick (Clicked (Digit 1)) ]
              [ text "1" ]
          , button [ class "r3 c1", onClick (Clicked (Digit 2)) ]
              [ text "2" ]
          , button [ class "r3 c2", onClick (Clicked (Digit 3)) ]
              [ text "3" ]
          , button [ class "r3 c3 rowspan2 bg-blue", onClick (Clicked Equal) ]
              [ text "=" ]
          , button [ class "r4 c0 colspan2", onClick (Clicked (Digit 0)) ]
              [ text "0" ]
          , button [ class "r4 c2", disabled True ]
              [ text "." ]
          ]
      ]


viewAttribution : Html msg
viewAttribution =
  footer [ class "attribution" ]
    [ text "Developed by "
    , a [ class "attribution__link"
        , href "https://github.com/dwayne/"
        , target "_blank"
        ]
        [ text "Dwayne Crooks" ]
    ]


-- HELPERS


nonBreakingSpace : Char
nonBreakingSpace = '\u{00A0}'
