module Main exposing (..)

import Browser
import Dict exposing (Dict)
import FontAwesome.Styles
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (src)
import Html.Styled.Events exposing (onClick, onDoubleClick)
import Http exposing (jsonBody)
import Json.Decode as JD exposing (Decoder, bool, decodeString, float, int, list, nullable, string)
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as JE
import StatusBar exposing (..)
import String exposing (concat)
import Styles
import Time



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
    { albumList : AlbumListModel
    , currentAlbum : Maybe Album
    , state : PlayerState
    , status : StatusBar.Model
    }


type AlbumListModel
    = Loading
    | Error
    | Albums (List Album)


type alias Album =
    { artist : String
    , title : String
    , coverPath : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { albumList = Loading
      , currentAlbum = Nothing
      , state = Stopped
      , status = StatusBar.init
      }
    , Cmd.batch [ loadAlbums, Cmd.map StatusBarMsg StatusBar.load ]
    )



-- UPDATE


type Msg
    = ReceivedAlbums (Result Http.Error (List Album))
    | AlbumChosen Album
    | StartedAlbum Album (Result Http.Error ())
    | StatusBarMsg StatusBar.Msg
    | Tick Time.Posix


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
                    ( { model | albumList = Error }, Cmd.none )

                Ok albums ->
                    ( { model | albumList = Albums albums }, Cmd.none )

        AlbumChosen album ->
            ( model, clearAndPlay album )

        StartedAlbum album result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok _ ->
                    ( { model | currentAlbum = Just album, state = Playing }, Cmd.map StatusBarMsg StatusBar.load )

        Tick time ->
            ( model, Cmd.map StatusBarMsg StatusBar.tick )



-- VIEW


view : Model -> Html Msg
view model =
    div [ Styles.body ]
        [ HS.fromUnstyled FontAwesome.Styles.css
        , div [] [ renderAlbumList model.albumList ]
        , HS.map StatusBarMsg (StatusBar.view model.status)
        ]


renderAlbumList : AlbumListModel -> Html Msg
renderAlbumList albumListModel =
    case albumListModel of
        Loading ->
            div [] [ text "Loading" ]

        Albums albums ->
            renderAlbumGrid albums

        Error ->
            div [] [ text "Error" ]


renderAlbumGrid : List Album -> Html Msg
renderAlbumGrid albums =
    div [ Styles.albumGrid ] (List.map renderAlbum (List.take 20 albums))


renderAlbum : Album -> Html Msg
renderAlbum album =
    -- This is a workaround for the current forwarding situation in Elm dev mode
    let
        fullPath =
            concat [ "/api", album.coverPath ]
    in
    li [ Styles.album, Styles.coverImage fullPath, onDoubleClick (AlbumChosen album) ]
        [ div [ Styles.description ]
            [ div [ Styles.title ] [ text album.title ]
            , div [ Styles.artist ] [ text album.artist ]
            ]
        ]


loadAlbums : Cmd Msg
loadAlbums =
    Http.get
        { url = "/api/albums"
        , expect = Http.expectJson ReceivedAlbums albumDecoder
        }


clearAndPlay : Album -> Cmd Msg
clearAndPlay album =
    Http.post
        { url = "/api/clear_and_play"
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "album", JE.string album.title )
                    , ( "album_artist", JE.string album.artist )
                    ]
                )
        , expect = Http.expectWhatever (StartedAlbum album)
        }


albumDecoder : JD.Decoder (List Album)
albumDecoder =
    JD.list
        (JD.map3 Album
            (JD.field "album_artist" JD.string)
            (JD.field "album" JD.string)
            (JD.field "cover_path" JD.string)
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 2000 Tick
