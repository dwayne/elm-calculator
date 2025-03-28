module Main exposing (main)

import Browser
import Data.Calculator as Calculator exposing (Calculator)
import Data.Key exposing (Key)
import Html as H
import View.Page as Page


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
            Calculator.toOutput calculator
    in
    Page.view
        { calculator =
            { line1 = line1
            , line2 = line2
            , onClick = Clicked
            }
        , attribution =
            { name = "Dwayne Crooks"
            , title = "Dwayne's GitHub profile"
            , url = "https://github.com/dwayne"
            }
        }
