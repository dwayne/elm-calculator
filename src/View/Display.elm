module View.Display exposing (ViewOptions, view)

import Html as H
import Html.Attributes as HA


type alias ViewOptions =
    { line1 : String
    , line2 : String
    }


view : ViewOptions -> H.Html msg
view { line1, line2 } =
    H.div [ HA.class "display" ]
        [ H.div [ HA.class "display__line1" ] [ H.text line1 ]
        , H.div [ HA.class "display__line2" ] [ H.text line2 ]
        ]
