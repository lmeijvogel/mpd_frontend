module PlayerDisplay exposing (..)

import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (disabled, href, selected, src, value)
import Html.Styled.Events exposing (onClick, onDoubleClick, onInput)
import Http exposing (jsonBody)
import Json.Decode as JD exposing (Decoder, bool, decodeString, float, int, list, nullable, string)
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as JE
import Player exposing (Player)
import PlayerDisplayStyles
import Responsive
import StatusBar exposing (..)
import String exposing (concat)
import Time


type alias Model =
    { player : Player
    , albumList : AlbumListModel
    , currentAlbum : Maybe Album
    , status : StatusBar.Model
    , visiblePage : VisiblePage
    }


type VisiblePage
    = AlbumsPage
    | StatusPage


type AlbumListModel
    = Loading
    | Error
    | Albums (List Album)


type alias Album =
    { artist : String
    , title : String
    , coverPath : String
    }


type Msg
    = ReceivedAlbums (Result Http.Error (List Album))
    | AlbumChosen Player Album
    | StartedAlbum Player Album (Result Http.Error ())
    | StatusBarMsg StatusBar.Msg


init : Player -> Model
init player =
    { player = player
    , albumList = Loading
    , currentAlbum = Nothing
    , status = StatusBar.init
    , visiblePage = AlbumsPage
    }


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

        AlbumChosen player album ->
            ( model, clearAndPlay player album )

        StartedAlbum player album result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok _ ->
                    ( { model | currentAlbum = Just album }, Cmd.map StatusBarMsg (StatusBar.load player) )


loadAlbumsAndStatus : Model -> Cmd Msg
loadAlbumsAndStatus model =
    Cmd.batch [ loadAlbums model.player, loadStatus model.player ]


triggerUpdateClock : Time.Posix -> Model -> Model
triggerUpdateClock time model =
    let
        currentStatus =
            model.status

        isPaused =
            model.status.playbackState.state == Paused

        newSecondsSinceLastUpdated =
            if isPaused then
                currentStatus.secondsSinceLastUpdate

            else
                currentStatus.secondsSinceLastUpdate + 1

        newStatus =
            { currentStatus | secondsSinceLastUpdate = newSecondsSinceLastUpdated }
    in
    { model | status = newStatus }


loadStatus : Player -> Cmd Msg
loadStatus player =
    Cmd.map StatusBarMsg (StatusBar.load player)


loadPlaybackState : Player -> Cmd Msg
loadPlaybackState player =
    Cmd.map StatusBarMsg (StatusBar.load player)


clearAndPlay : Player -> Album -> Cmd Msg
clearAndPlay player album =
    Http.post
        { url = buildPlayerUrl "clear_and_play" player
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "album", JE.string album.title )
                    , ( "album_artist", JE.string album.artist )
                    ]
                )
        , expect = Http.expectWhatever (StartedAlbum player album)
        }


loadAlbums : Player -> Cmd Msg
loadAlbums player =
    Http.get
        { url = buildPlayerUrl "albums" player
        , expect = Http.expectJson ReceivedAlbums albumDecoder
        }


albumDecoder : JD.Decoder (List Album)
albumDecoder =
    JD.list
        (JD.map3 Album
            (JD.field "album_artist" JD.string)
            (JD.field "album" JD.string)
            (JD.field "cover_path" JD.string)
        )


view : Model -> Responsive.ClientType -> Html Msg
view model clientType =
    let
        mainPage =
            case model.visiblePage of
                AlbumsPage ->
                    renderAlbums model clientType

                StatusPage ->
                    renderStatusBar model.player model.status clientType

        bottomBar =
            HS.map StatusBarMsg (StatusBar.renderStatusSummary model.player model.status clientType)
    in
    div [] [ mainPage, bottomBar ]


renderAlbums : Model -> Responsive.ClientType -> Html Msg
renderAlbums model clientType =
    case model.albumList of
        Loading ->
            div [] [ text "Loading" ]

        Albums albums ->
            case clientType of
                Responsive.Desktop ->
                    renderAlbumGrid model.player albums

                Responsive.Mobile ->
                    renderAlbumList model.player albums

                Responsive.Unknown ->
                    div [] []

        Error ->
            div [] [ text "Error" ]


renderAlbumGrid : Player -> List Album -> Html Msg
renderAlbumGrid player albums =
    div [ PlayerDisplayStyles.albumGrid ] (List.map (renderAlbumTile player) (List.take 30 albums))


renderAlbumTile : Player -> Album -> Html Msg
renderAlbumTile player album =
    -- This is a workaround for the current forwarding situation in Elm dev mode
    let
        fullPath =
            concat [ "/api", album.coverPath ]
    in
    li [ PlayerDisplayStyles.albumTile, PlayerDisplayStyles.coverImage fullPath, onDoubleClick (AlbumChosen player album) ]
        [ div [ PlayerDisplayStyles.description ]
            [ div [ PlayerDisplayStyles.title ] [ text album.title ]
            , div [ PlayerDisplayStyles.artist ] [ text album.artist ]
            ]
        ]


renderAlbumList : Player -> List Album -> Html Msg
renderAlbumList player albums =
    ul [ PlayerDisplayStyles.albumList ] (List.map (renderAlbumLine player) (List.take 30 albums))


renderAlbumLine : Player -> Album -> Html Msg
renderAlbumLine player album =
    -- This is a workaround for the current forwarding situation in Elm dev mode
    let
        fullPath =
            concat [ "/api", album.coverPath ]
    in
    -- AlbumLines are only rendered on mobile, so it must respond to onClick instead of
    -- onDoubleClick
    li [ PlayerDisplayStyles.albumListItem, onClick (AlbumChosen player album) ]
        [ span [ PlayerDisplayStyles.albumLineCoverImage fullPath ] []
        , span [ PlayerDisplayStyles.albumDescription ]
            [ div [ PlayerDisplayStyles.albumLineTitle ] [ text album.title ]
            , div [ PlayerDisplayStyles.albumLineArtist ] [ text album.artist ]
            ]
        ]


renderStatusBar : Player -> StatusBar.Model -> Responsive.ClientType -> Html Msg
renderStatusBar player status clientType =
    HS.map StatusBarMsg (StatusBar.view player status clientType)
