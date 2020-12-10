module StatusBarStyles exposing (..)

import Css exposing (..)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)


panel : Attribute msg
panel =
    css
        [ position fixed
        , bottom (px 0)
        , minHeight (px 45)
        , width (pct 100)
        , backgroundColor (rgb 64 64 64)
        , color (rgb 200 200 200)
        ]


mainContents : Bool -> Attribute msg
mainContents showContents =
    if showContents then
        css
            [ displayFlex
            , flexDirection row
            ]

    else
        css [ display none ]


topBar : Attribute msg
topBar =
    css
        [ displayFlex
        , justifyContent spaceBetween
        , alignItems center
        , flexDirection row
        , height (px 45)
        , padding2 (px 0) (px 10)
        ]


playlist : Attribute msg
playlist =
    css
        [ height (px 170)
        , margin (px 0)
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
        , margin2 (px 16) (px 8)
        , color (rgb 128 128 128)
        ]


activeButton : Attribute msg
activeButton =
    css
        [ color (rgb 255 255 255)
        ]
