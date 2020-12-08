module StatusBarStyles exposing (..)

import Css exposing (..)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)


mainContents : Attribute msg
mainContents =
    css
        [ displayFlex
        , flexDirection row
        ]


topBar : Attribute msg
topBar =
    css
        [ width (pct 100)
        ]


selected : Attribute msg
selected =
    css
        [ backgroundColor (rgb 0 0 0)
        , color (rgb 255 255 255)
        ]


controlButton : Attribute msg
controlButton =
    css
        [ fontSize (pt 20)
        , margin (px 8)
        ]
