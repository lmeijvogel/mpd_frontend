module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (src)
import Html.Styled.Events exposing (onClick)

import Http
import Json.Decode exposing (Decoder, field, list, map3, string)
import String exposing (concat)
import Styles



-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view >> toUnstyled
    }



-- MODEL

type alias Cd = {
  artist: String
  , title: String
  , coverPath: String
  }

type alias Model = {
  cdList: CdListModel
  }

type CdListModel
  = Loading
  | Error
  | CDs (List Cd)

init : () -> (Model, Cmd Msg)
init _ = ({ cdList =  Loading }, loadAlbums )

-- UPDATE


type Msg
  = ReceivedAlbums (Result Http.Error (List Cd))
  -- | CdSelected Cd


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceivedAlbums result ->
      case result of
        Err _ -> ({ model | cdList = Error}, Cmd.none)
        Ok cds -> ({ model | cdList = CDs cds}, Cmd.none)
    -- CdSelected cd ->


-- VIEW


view : Model -> Html Msg
view model =
  div [Styles.body]
    [ div [] [renderCdList model.cdList] ]

renderCdList : CdListModel -> Html Msg
renderCdList cdListModel =
  case cdListModel of
    Loading -> div [] [ text "Loading" ]
    CDs cds -> renderCdGrid cds
    Error -> div [] [ text "Error" ]

renderCdGrid : List Cd -> Html Msg
renderCdGrid cds =
    div [Styles.cdGrid] (List.map renderCd (List.take 20 cds))

renderCd : Cd -> Html Msg
renderCd cd =
  -- This is a workaround for the current forwarding situation in Elm dev mode
  let fullPath = concat ["/api", cd.coverPath]
  in
  li [Styles.cd, (Styles.coverImage fullPath)] [
    div [Styles.description] [
      div [Styles.title] [text cd.title]
    , div [Styles.artist] [text cd.artist]
    ]
  ]

loadAlbums : Cmd Msg
loadAlbums =
  Http.get
    {
      url = "/api/albums"
    , expect = Http.expectJson ReceivedAlbums albumDecoder
    }

albumDecoder : Decoder (List Cd)
albumDecoder =
  (list (map3 Cd
    (field "album" string)
    (field "album_artist" string)
    (field "cover_path" string))
  )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
