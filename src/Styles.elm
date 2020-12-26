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


albumGrid : Attribute msg
albumGrid =
    css
        [ width (pct 100)
        , backgroundColor (rgb 0 0 0)
        , color (rgb 255 255 255)
        , displayFlex
        , flexWrap wrap
        ]


albumTile : Attribute msg
albumTile =
    css
        [ displayFlex
        , flexDirection column
        , justifyContent flexEnd
        , margin (px 10)
        , width (px 200)
        , height (px 200)
        , fontSize (pt 14)
        , cursor pointer
        ]


albumList : Attribute msg
albumList =
    css
        [ displayFlex
        , flexDirection column
        , width (pct 100)
        , textAlign left
        , fontSize (vw 4)
        , listStyle none
        , backgroundColor (rgb 0 0 0)
        , color (rgb 255 255 255)
        , displayFlex
        , flexWrap wrap
        ]


albumListItem : Attribute msg
albumListItem =
    css
        [ displayFlex
        , flexDirection row
        , alignItems center
        , margin2 (px 5) (px 0)
        , cursor pointer
        , property "user-select" "none"
        ]


albumDescription : Attribute msg
albumDescription =
    css
        [ displayFlex
        , flexDirection column
        ]


albumLineTitle : Attribute msg
albumLineTitle =
    css [ color (rgb 255 255 255) ]


albumLineArtist : Attribute msg
albumLineArtist =
    css [ color (rgb 160 160 160) ]


albumLineCoverImage : String -> Attribute msg
albumLineCoverImage coverPath =
    let
        imageComponent =
            String.concat [ "url(", coverPath, ")" ]
    in
    css
        [ width (vw 20)
        , height (vw 20)
        , margin2 (px 0) (px 20)
        , property "background-image" imageComponent
        , backgroundSize cover
        ]


coverImage : String -> Attribute msg
coverImage coverPath =
    let
        imageComponent =
            String.concat [ "url(", coverPath, ")" ]
    in
    css
        [ property "background-image" imageComponent
        , backgroundSize cover
        ]


description : Attribute msg
description =
    css
        [ displayFlex
        , flexDirection column
        , alignItems center
        , bottom (px 0)
        , padding3 (px 10) (px 5) (px 2)
        , backgroundColor (rgb 0 0 0)
        , property "background" "linear-gradient(0deg, rgba(0, 0, 0, 0.6) 0%, rgba(0, 0, 0, 0.6) 64%, rgba(0, 0, 0, 0) 100%)"
        , property "user-select" "none"
        ]


title : Attribute msg
title =
    css
        [ fontStyle italic
        , textAlign center
        , fontSize (pt 16)
        ]


artist : Attribute msg
artist =
    css
        [ width (pct 100)
        , textAlign right
        , fontSize (pt 12)
        ]


albumCover : Attribute msg
albumCover =
    css
        [ width (px 200)
        , height (px 200)
        ]
