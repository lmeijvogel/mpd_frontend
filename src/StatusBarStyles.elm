module StatusBarStyles exposing (..)

import Css exposing (..)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)


panelBackgroundColor : Color
panelBackgroundColor =
    rgb 64 64 64


mainContents : Attribute msg
mainContents =
    css
        [ displayFlex
        , flexDirection column
        , justifyContent spaceBetween
        , width (pct 100)
        , height (pct 100)
        , backgroundColor panelBackgroundColor
        , property "user-select" "none"
        ]


topBar : Attribute msg
topBar =
    css
        [ displayFlex
        , position fixed
        , bottom (px 0)
        , minHeight (px 45)
        , width (pct 100)
        , backgroundColor panelBackgroundColor
        , color (rgb 200 200 200)
        , property "user-select" "none"
        , justifyContent spaceBetween
        , alignItems center
        , flexDirection row
        , height (px 45)
        ]


playlist : Attribute msg
playlist =
    css
        [ overflowY scroll
        , textAlign left
        , fontSize (pt 15)
        , flexGrow (int 2)
        ]


playlistEntry : Attribute msg
playlistEntry =
    css
        [ marginBottom (px 10)
        , paddingLeft (px 10)
        ]


playlistTitle : Attribute msg
playlistTitle =
    css
        [ color (rgb 255 255 255)
        ]


playlistArtist : Attribute msg
playlistArtist =
    css
        [ color (rgb 160 160 160)
        ]


selected : Attribute msg
selected =
    css
        [ backgroundColor (rgb 0 0 0)
        , color (rgb 255 255 255)
        ]



-- These styles should ideally have been set on the containing element, topBar,
-- but according to https://stackoverflow.com/a/50874488, flexbox first determines
-- the sizes of the contents, then places the elements and only then adds padding.
-- So we set the paddings on the contents instead.


controlButtons : Attribute msg
controlButtons =
    css [ paddingLeft (px 10) ]


showHideButton : Attribute msg
showHideButton =
    css [ paddingRight (px 10) ]


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


outputRow =
    css
        [ listStyle none
        , textAlign left
        ]


output : Attribute msg
output =
    css
        [ textAlign left
        , border (px 0)
        , backgroundColor panelBackgroundColor
        ]


inactiveOutput : Attribute msg
inactiveOutput =
    css
        [ color (rgb 128 128 128)
        ]


activeOutput : Attribute msg
activeOutput =
    css
        [ color (rgb 255 255 255)
        ]
