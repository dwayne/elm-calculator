module View.Calculator exposing (ViewOptions, view)

import Data.Key exposing (Key)
import Html as H
import Html.Attributes as HA
import View.Display as Display
import View.Pad as Pad


type alias ViewOptions msg =
    { line1 : String
    , line2 : String
    , onClick : Key -> msg
    }


view : ViewOptions msg -> H.Html msg
view { line1, line2, onClick } =
    H.div [ HA.class "calculator" ]
        [ H.div
            [ HA.class "calculator__display" ]
            [ Display.view
                { line1 = line1
                , line2 = line2
                }
            ]
        , H.div
            [ HA.class "calculator__pad" ]
            [ Pad.view onClick ]
        ]
