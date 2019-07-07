module Main exposing (main)


import Html exposing (Html, a, button, div, footer, text)
import Html.Attributes exposing (class, href, target)


main : Html msg
main =
  view


-- VIEW


view : Html msg
view =
  div [ class "inline-block" ]
    [ viewCalculator
    , viewAttribution
    ]


viewCalculator : Html msg
viewCalculator =
  div [ class "calculator" ]
    [ div [ class "calculator__expr" ] [ text "1+2=3" ]
    , div [ class "calculator__output" ] [ text "3" ]
    , div [ class "calculator__buttons" ]
        [ button [ class "r0 c0 colspan2 bg-red" ]
            [ text "AC" ]
        , button [ class "r0 c2" ]
            [ text "÷" ]
        , button [ class "r0 c3" ]
            [ text "×" ]
        , button [ class "r1 c0" ]
            [ text "7" ]
        , button [ class "r1 c1" ]
            [ text "8" ]
        , button [ class "r1 c2" ]
            [ text "9" ]
        , button [ class "r1 c3" ]
            [ text "-" ]
        , button [ class "r2 c0" ]
            [ text "4" ]
        , button [ class "r2 c1" ]
            [ text "5" ]
        , button [ class "r2 c2" ]
            [ text "6" ]
        , button [ class "r2 c3" ]
            [ text "+" ]
        , button [ class "r3 c0" ]
            [ text "1" ]
        , button [ class "r3 c1" ]
            [ text "2" ]
        , button [ class "r3 c2" ]
            [ text "3" ]
        , button [ class "r3 c3 rowspan2 bg-blue" ]
            [ text "=" ]
        , button [ class "r4 c0 colspan2" ]
            [ text "0" ]
        , button [ class "r4 c2" ]
            [ text "." ]
        ]
    ]


viewAttribution : Html msg
viewAttribution =
  footer [ class "attribution" ]
    [ text "Developed by"
    , a [ class "attribution__link"
        , href "https://github.com/dwayne/"
        , target "_blank"
        ]
        [ text "Dwayne Crooks" ]
    ]
