module Styles exposing (..)

import Css exposing (..)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)


body : Attribute msg
body =
    css
        [ width (pct 100)
        , textAlign center
        , backgroundColor (rgb 0 0 0)
        , color (rgb 255 255 255)
        , height (vh 100)
        ]


playerSelector : Attribute msg
playerSelector =
    css
        [ displayFlex
        , flexDirection row
        , height (px 50)
        , justifyContent flexEnd
        , alignItems center
        , listStyle none
        , margin (px 0)
        , padding (px 0)
        ]


player : Attribute msg
player =
    css
        [ margin2 (px 0) (px 5)
        ]


playerLink : Attribute msg
playerLink =
    css
        [ backgroundColor (rgb 0 0 0)
        , color (rgb 255 255 255)
        , textDecoration none
        , padding2 (px 10) (px 10)
        ]


selectedPlayer : Attribute msg
selectedPlayer =
    css
        [ backgroundColor (rgb 255 255 255)
        , color (rgb 0 0 0)
        ]
