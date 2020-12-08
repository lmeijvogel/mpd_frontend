module StatusBarStyles exposing (..)

import Css exposing (..)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)


panel : Attribute msg
panel =
    css
        [ position fixed
        , bottom (px 0)
        , minHeight (px 40)
        , width (pct 100)
        , backgroundColor (rgb 64 64 64)
        , color (rgb 200 200 200)
        ]


mainContents : Attribute msg
mainContents =
    css
        [ displayFlex
        , flexDirection row
        ]


topBar : Attribute msg
topBar =
    css
        [ displayFlex
        , justifyContent spaceBetween
        , alignItems center
        , flexDirection row
        , width (pct 100)
        ]


playlist : Attribute msg
playlist =
    css
        [ height (px 170)
        , overflowY scroll
        , listStyle none
        , textAlign left
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
