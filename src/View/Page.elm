module View.Page exposing (ViewOptions, view)

import Html as H
import Html.Attributes as HA
import View.Attribution as Attribution
import View.Calculator as Calculator


type alias ViewOptions msg =
    { calculator : Calculator.ViewOptions msg
    , attribution : Attribution.ViewOptions
    }


view : ViewOptions msg -> H.Html msg
view { calculator, attribution } =
    H.div [ HA.class "page" ]
        [ H.div [ HA.class "page__wrapper" ]
            [ H.div [ HA.class "page__content" ]
                [ H.main_ [] [ Calculator.view calculator ]
                , H.footer [] [ Attribution.view attribution ]
                ]
            ]
        ]
