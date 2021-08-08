module StatusPageStyles exposing (..)

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
        , flexDirection row
        , justifyContent spaceBetween
        , alignItems center
        , width (pct 100)
        , backgroundColor panelBackgroundColor
        , property "height" "calc(100% - 45px - 50px)"
        , property "user-select" "none"
        , overflow hidden
        ]


playlistContainer : Attribute msg
playlistContainer =
    css
        [ textAlign left
        , fontSize (pt 15)
        , height (pct 100)
        , overflowY scroll
        ]


playlist : Attribute msg
playlist =
    css
        [ displayFlex
        , flexDirection column
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


controls : Attribute msg
controls =
    css
        [ displayFlex
        , flexDirection column
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
        [ fontSize (pt 25)
        , padding2 (px 8) (px 4)
        , color (rgb 128 128 128)
        ]


playModeButton : Attribute msg
playModeButton =
    css
        [ padding2 (px 8) (px 4)
        ]


inactiveButton : Attribute msg
inactiveButton =
    css
        [ color (rgb 128 128 128)
        ]


activeButton : Attribute msg
activeButton =
    css
        [ color (rgb 255 255 255)
        ]


outputRow : Attribute msg
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


statusSummary : Attribute msg
statusSummary =
    css
        [ minHeight (px 45)
        , displayFlex
        , flexDirection row
        , backgroundColor panelBackgroundColor
        , color (rgb 200 200 200)
        , property "user-select" "none"
        , justifyContent spaceBetween
        , alignItems center
        , height (px 45)
        , width (pct 100)
        ]
