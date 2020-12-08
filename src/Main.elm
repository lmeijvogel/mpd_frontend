module Main exposing (..)

import Browser
import Dict exposing (Dict)
import FontAwesome.Styles
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (src)
import Html.Styled.Events exposing (onClick)
import Http exposing (jsonBody)
import Json.Decode as JD exposing (Decoder, bool, decodeString, float, int, list, nullable, string)
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as JE
import StatusBar exposing (..)
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


type alias Model =
    { cdList : CdListModel
    , currentCd : Maybe Cd
    , state : PlayerState
    , status : StatusBar.Model
    }


type CdListModel
    = Loading
    | Error
    | CDs (List Cd)


type alias Cd =
    { artist : String
    , title : String
    , coverPath : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cdList = Loading
      , currentCd = Nothing
      , state = Stopped
      , status = StatusBar.init
      }
    , Cmd.batch [ loadAlbums, Cmd.map StatusBarMsg StatusBar.load ]
    )



-- UPDATE


type Msg
    = ReceivedAlbums (Result Http.Error (List Cd))
    | CdChosen Cd
    | StartedAlbum Cd (Result Http.Error ())
    | StatusBarMsg StatusBar.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StatusBarMsg message ->
            let
                ( newStatusBarModel, action ) =
                    StatusBar.update message model.status
            in
            ( { model | status = newStatusBarModel }, Cmd.map StatusBarMsg action )

        ReceivedAlbums result ->
            case result of
                Err _ ->
                    ( { model | cdList = Error }, Cmd.none )

                Ok cds ->
                    ( { model | cdList = CDs cds }, Cmd.none )

        CdChosen cd ->
            ( model, clearAndPlay cd )

        StartedAlbum cd result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok _ ->
                    ( { model | currentCd = Just cd, state = Playing }, Cmd.map StatusBarMsg StatusBar.load )



-- VIEW


view : Model -> Html Msg
view model =
    div [ Styles.body ]
        [ HS.fromUnstyled FontAwesome.Styles.css
        , div [] [ renderCdList model.cdList ]
        , HS.map StatusBarMsg (StatusBar.view model.status)
        ]


renderCdList : CdListModel -> Html Msg
renderCdList cdListModel =
    case cdListModel of
        Loading ->
            div [] [ text "Loading" ]

        CDs cds ->
            renderCdGrid cds

        Error ->
            div [] [ text "Error" ]


renderCdGrid : List Cd -> Html Msg
renderCdGrid cds =
    div [ Styles.cdGrid ] (List.map renderCd (List.take 20 cds))


renderCd : Cd -> Html Msg
renderCd cd =
    -- This is a workaround for the current forwarding situation in Elm dev mode
    let
        fullPath =
            concat [ "/api", cd.coverPath ]
    in
    li [ Styles.cd, Styles.coverImage fullPath, onClick (CdChosen cd) ]
        [ div [ Styles.description ]
            [ div [ Styles.title ] [ text cd.title ]
            , div [ Styles.artist ] [ text cd.artist ]
            ]
        ]


loadAlbums : Cmd Msg
loadAlbums =
    Http.get
        { url = "/api/albums"
        , expect = Http.expectJson ReceivedAlbums albumDecoder
        }


clearAndPlay : Cd -> Cmd Msg
clearAndPlay cd =
    Http.post
        { url = "/api/clear_and_play"
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "album", JE.string cd.title )
                    , ( "album_artist", JE.string cd.artist )
                    ]
                )
        , expect = Http.expectWhatever (StartedAlbum cd)
        }


albumDecoder : JD.Decoder (List Cd)
albumDecoder =
    JD.list
        (JD.map3 Cd
            (JD.field "album_artist" JD.string)
            (JD.field "album" JD.string)
            (JD.field "cover_path" JD.string)
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
