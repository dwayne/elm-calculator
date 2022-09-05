module Main exposing (main)


import Browser
import Calculator exposing (Calculator)
import Html as H
import Html.Attributes as HA
import Key exposing (Key)


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
  viewLayout
    { calculator = Calculator.view Clicked calculator
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
