module Main exposing (main)


import Browser
import Calculator exposing (Calculator)
import Digit
import Html as H
import Html.Attributes as HA
import Key exposing (Key)
import Operator


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
      { model | calculator = Calculator.press key model.calculator }


-- VIEW


view : Model -> H.Html Msg
view { calculator } =
  let
    { line1, line2 } =
      Calculator.toDisplay calculator
  in
  viewLayout
    { calculator =
        viewCalculator
          { line1 = line1
          , line2 = line2
          , onClick = Clicked
          }
    , attribution =
        viewAttribution
          { name = "Dwayne Crooks"
          , url = "https://github.com/dwayne"
          }
    }


viewLayout :
  { calculator : H.Html msg
  , attribution : H.Html msg
  }
  -> H.Html msg
viewLayout { calculator, attribution } =
  H.div [ HA.class "layout" ]
    [ H.div [ HA.class "layout__wrapper" ]
        [ H.div [ HA.class "layout__main" ]
            [ H.div [ HA.class "layout__calculator" ] [ calculator ]
            , H.div [ HA.class "layout__attribution" ] [ attribution ]
            ]
        ]
    ]


viewCalculator :
  { line1 : String
  , line2 : String
  , onClick : Key -> msg
  }
  -> H.Html msg
viewCalculator { line1, line2, onClick } =
  H.div [ HA.class "calculator" ]
    [ H.div [ HA.class "calculator__display" ]
        [ viewDisplay
            { line1 = line1
            , line2 = line2
            }
        ]
    , H.div [ HA.class "calculator__pad" ]
        [ viewPad onClick
        ]
    ]


viewDisplay :
  { line1 : String
  , line2 : String
  }
  -> H.Html msg
viewDisplay { line1, line2 } =
  H.div [ HA.class "display" ]
    [ H.div [ HA.class "display__line1" ] [ H.text line1 ]
    , H.div [ HA.class "display__line2" ] [ H.text line2 ]
    ]


viewPad : (Key -> msg) -> H.Html msg
viewPad onClick =
  H.div [ HA.class "pad" ]
    [ viewPadSlot
        { position = (0, 0)
        , span = Just Colspan
        , keyOptions =
            { style = Key.Primary
            , onClick = onClick
            }
        , key = Key.AC
        }
    , viewPadSlot
        { position = (0, 2)
        , span = Nothing
        , keyOptions =
            { style = Key.Default
            , onClick = onClick
            }
        , key = Key.Operator Operator.Div
        }
    , viewPadSlot
        { position = (0, 3)
        , span = Nothing
        , keyOptions =
            { style = Key.Default
            , onClick = onClick
            }
        , key = Key.Operator Operator.Mul
        }
    , viewPadSlot
        { position = (1, 0)
        , span = Nothing
        , keyOptions =
            { style = Key.Default
            , onClick = onClick
            }
        , key = Key.Digit Digit.Seven
        }
    , viewPadSlot
        { position = (1, 1)
        , span = Nothing
        , keyOptions =
            { style = Key.Default
            , onClick = onClick
            }
        , key = Key.Digit Digit.Eight
        }
    , viewPadSlot
        { position = (1, 2)
        , span = Nothing
        , keyOptions =
            { style = Key.Default
            , onClick = onClick
            }
        , key = Key.Digit Digit.Nine
        }
    , viewPadSlot
        { position = (1, 3)
        , span = Nothing
        , keyOptions =
            { style = Key.Default
            , onClick = onClick
            }
        , key = Key.Operator Operator.Sub
        }
    , viewPadSlot
        { position = (2, 0)
        , span = Nothing
        , keyOptions =
            { style = Key.Default
            , onClick = onClick
            }
        , key = Key.Digit Digit.Four
        }
    , viewPadSlot
        { position = (2, 1)
        , span = Nothing
        , keyOptions =
            { style = Key.Default
            , onClick = onClick
            }
        , key = Key.Digit Digit.Five
        }
    , viewPadSlot
        { position = (2, 2)
        , span = Nothing
        , keyOptions =
            { style = Key.Default
            , onClick = onClick
            }
        , key = Key.Digit Digit.Six
        }
    , viewPadSlot
        { position = (2, 3)
        , span = Nothing
        , keyOptions =
            { style = Key.Default
            , onClick = onClick
            }
        , key = Key.Operator Operator.Add
        }
    , viewPadSlot
        { position = (3, 0)
        , span = Nothing
        , keyOptions =
            { style = Key.Default
            , onClick = onClick
            }
        , key = Key.Digit Digit.One
        }
    , viewPadSlot
        { position = (3, 1)
        , span = Nothing
        , keyOptions =
            { style = Key.Default
            , onClick = onClick
            }
        , key = Key.Digit Digit.Two
        }
    , viewPadSlot
        { position = (3, 2)
        , span = Nothing
        , keyOptions =
            { style = Key.Default
            , onClick = onClick
            }
        , key = Key.Digit Digit.Three
        }
    , viewPadSlot
        { position = (3, 3)
        , span = Just Rowspan
        , keyOptions =
            { style = Key.Secondary
            , onClick = onClick
            }
        , key = Key.Equal
        }
    , viewPadSlot
        { position = (4, 0)
        , span = Just Colspan
        , keyOptions =
            { style = Key.Default
            , onClick = onClick
            }
        , key = Key.Digit Digit.Zero
        }
    , viewPadSlot
        { position = (4, 2)
        , span = Nothing
        , keyOptions =
            { style = Key.Default
            , onClick = onClick
            }
        , key = Key.Dot
        }
    ]


type Span
  = Colspan
  | Rowspan


viewPadSlot :
  { position : (Int, Int)
  , span : Maybe Span
  , keyOptions : Key.Options msg
  , key : Key
  }
  -> H.Html msg
viewPadSlot { position, span, keyOptions, key } =
  let
    (r, c) =
      position
  in
  H.div
    [ HA.class "pad__slot"
    , HA.class <| "r" ++ String.fromInt r
    , HA.class <| "c" ++ String.fromInt c
    , HA.class <|
        case span of
          Nothing ->
            ""

          Just Colspan ->
            "colspan2"

          Just Rowspan ->
            "rowspan2"
    ]
    [ Key.view keyOptions key ]


viewAttribution :
  { name : String
  , url : String
  }
  -> H.Html msg
viewAttribution { name, url } =
  H.p [ HA.class "attribution" ]
    [ H.text "Developed by "
    , H.a
        [ HA.href url
        , HA.target "_blank"
        ]
        [ H.text name ]
    ]
