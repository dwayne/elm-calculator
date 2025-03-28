module View.Attribution exposing (ViewOptions, view)

import Html as H
import Html.Attributes as HA


type alias ViewOptions =
    { name : String
    , title : String
    , url : String
    }


view : ViewOptions -> H.Html msg
view { name, title, url } =
    H.p [ HA.class "attribution" ]
        [ H.text "Developed by "
        , H.a
            [ HA.class "attribution__link"
            , HA.href url
            , HA.target "_blank"
            , HA.title title
            ]
            [ H.text name ]
        ]
