module View.Pad exposing (view)

import Data.Digit as Digit
import Data.Key as Key exposing (Key)
import Data.Operator as Operator
import Html as H
import Html.Attributes as HA
import View.Key as Key


view : (Key -> msg) -> H.Html msg
view onClick =
    H.div [ HA.class "pad" ]
        [ viewPadSlot
            { position = ( 0, 0 )
            , span = Just Colspan
            , style = Key.Primary
            , key = Key.AC
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 0, 2 )
            , span = Nothing
            , style = Key.Default
            , key = Key.Operator Operator.Div
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 0, 3 )
            , span = Nothing
            , style = Key.Default
            , key = Key.Operator Operator.Mul
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 1, 0 )
            , span = Nothing
            , style = Key.Default
            , key = Key.Digit Digit.Seven
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 1, 1 )
            , span = Nothing
            , style = Key.Default
            , key = Key.Digit Digit.Eight
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 1, 2 )
            , span = Nothing
            , style = Key.Default
            , key = Key.Digit Digit.Nine
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 1, 3 )
            , span = Nothing
            , style = Key.Default
            , key = Key.Operator Operator.Sub
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 2, 0 )
            , span = Nothing
            , style = Key.Default
            , key = Key.Digit Digit.Four
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 2, 1 )
            , span = Nothing
            , style = Key.Default
            , key = Key.Digit Digit.Five
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 2, 2 )
            , span = Nothing
            , style = Key.Default
            , key = Key.Digit Digit.Six
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 2, 3 )
            , span = Nothing
            , style = Key.Default
            , key = Key.Operator Operator.Add
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 3, 0 )
            , span = Nothing
            , style = Key.Default
            , key = Key.Digit Digit.One
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 3, 1 )
            , span = Nothing
            , style = Key.Default
            , key = Key.Digit Digit.Two
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 3, 2 )
            , span = Nothing
            , style = Key.Default
            , key = Key.Digit Digit.Three
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 3, 3 )
            , span = Just Rowspan
            , style = Key.Secondary
            , key = Key.Equal
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 4, 0 )
            , span = Just Colspan
            , style = Key.Default
            , key = Key.Digit Digit.Zero
            , onClick = onClick
            }
        , viewPadSlot
            { position = ( 4, 2 )
            , span = Nothing
            , style = Key.Default
            , key = Key.Dot
            , onClick = onClick
            }
        ]


type Span
    = Colspan
    | Rowspan


viewPadSlot :
    { position : ( Int, Int )
    , span : Maybe Span
    , style : Key.Style
    , key : Key
    , onClick : Key -> msg
    }
    -> H.Html msg
viewPadSlot { position, span, style, key, onClick } =
    let
        ( r, c ) =
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
        [ Key.view
            { style = style
            , key = key
            , onClick = onClick
            }
        ]
