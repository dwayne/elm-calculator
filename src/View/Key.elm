module View.Key exposing (Style(..), ViewOptions, view)

import Data.Key as Key exposing (Key)
import Html as H
import Html.Attributes as HA
import Html.Events as HE


type alias ViewOptions msg =
    { style : Style
    , key : Key
    , onClick : Key -> msg
    }


type Style
    = Default
    | Primary
    | Secondary


view : ViewOptions msg -> H.Html msg
view { style, key, onClick } =
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
        , HA.type_ "button"
        , HE.onClick <| onClick key
        ]
        [ H.text <| Key.toString key ]
