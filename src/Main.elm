module Main exposing (..)

import Browser
import Dict exposing (Dict)
import FontAwesome.Styles
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (disabled, selected, src, value)
import Html.Styled.Events exposing (onClick, onDoubleClick, onInput)
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
    { playerList : PlayersListModel
    , albumList : AlbumListModel
    , currentAlbum : Maybe Album
    , state : PlayerState
    , status : StatusBar.Model
    }


type PlayersListModel
    = PlayersLoading
    | PlayersError
    | Players (List Player)


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
    ( { playerList = PlayersLoading
      , albumList = Loading
      , currentAlbum = Nothing
      , state = Stopped
      , status = StatusBar.init
      }
      -- Players are only loaded once to fix strange results
      -- when the player selector dropdown changed by removing
      -- the 'no player selected' option.
    , Cmd.batch [ loadPlayers, fireInitCmds ]
    )


fireInitCmds =
    Cmd.batch [ loadAlbums, loadStatus ]



-- UPDATE


type Msg
    = ReceivedPlayers (Result Http.Error (List Player))
    | PlayerChosen String
    | SelectedPlayer String (Result Http.Error ())
    | ReceivedAlbums (Result Http.Error (List Album))
    | AlbumChosen Album
    | StartedAlbum Album (Result Http.Error ())
    | StatusBarMsg StatusBar.Msg
    | TriggerRetrieveStatus Time.Posix
    | TriggerUpdateClock Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedPlayers result ->
            case result of
                Err _ ->
                    ( { model | playerList = PlayersError }, Cmd.none )

                Ok players ->
                    ( { model | playerList = Players players }, Cmd.none )

        PlayerChosen ip ->
            let
                newStatus =
                    replacePlayerInStatus model ip
            in
            ( { model | status = newStatus }, selectPlayer ip )

        SelectedPlayer ip result ->
            let
                playbackState =
                    model.status.playbackState

                players =
                    case model.playerList of
                        Players p ->
                            p

                        _ ->
                            []
            in
            case result of
                Err _ ->
                    let
                        newPlaybackState =
                            { playbackState | player = Nothing }
                    in
                    ( model, fireInitCmds )

                Ok _ ->
                    ( { model | status = replacePlayerInStatus model ip }, fireInitCmds )

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

        TriggerRetrieveStatus time ->
            ( model, Cmd.map StatusBarMsg StatusBar.loadPlaybackState )

        TriggerUpdateClock time ->
            let
                currentStatus =
                    model.status

                newSecondsSinceLastUpdated =
                    if isPaused model then
                        currentStatus.secondsSinceLastUpdate

                    else
                        currentStatus.secondsSinceLastUpdate + 1

                newStatus =
                    { currentStatus | secondsSinceLastUpdate = newSecondsSinceLastUpdated }
            in
            ( { model | status = newStatus }, Cmd.none )


replacePlayerInStatus : Model -> String -> StatusBar.Model
replacePlayerInStatus model ip =
    let
        playbackState =
            model.status.playbackState

        players =
            case model.playerList of
                Players p ->
                    p

                _ ->
                    []

        player =
            List.filter (\p -> p.ip == ip) players |> List.head

        newPlaybackState =
            { playbackState | player = player }

        status =
            model.status
    in
    { status | playbackState = newPlaybackState }


isPaused : Model -> Bool
isPaused model =
    model.state == Paused



-- VIEW


view : Model -> Html Msg
view model =
    div [ Styles.body ]
        [ HS.fromUnstyled FontAwesome.Styles.css
        , renderPlayerSelector model
        , div [] [ renderAlbumList model.albumList ]
        , HS.map StatusBarMsg (StatusBar.view model.status)
        ]


renderPlayerSelector : Model -> Html Msg
renderPlayerSelector model =
    case model.playerList of
        PlayersLoading ->
            span [] [ text "Loading ..." ]

        PlayersError ->
            span [] [ text "Error loading players ..." ]

        Players players ->
            let
                maybeCurrentPlayer =
                    model.status.playbackState.player
            in
            div [ Styles.playerSelector ]
                [ select [ onInput PlayerChosen ]
                    (buildPlayerOptions players maybeCurrentPlayer)
                ]


buildPlayerOptions players maybeCurrentPlayer =
    let
        options =
            List.map (renderPlayerOption maybeCurrentPlayer) players
    in
    case maybeCurrentPlayer of
        Just currentPlayer ->
            options

        Nothing ->
            [ renderNullPlayerOption ] ++ options


renderPlayerOption : Maybe Player -> Player -> Html Msg
renderPlayerOption maybeCurrentPlayer player =
    let
        isSelected =
            case maybeCurrentPlayer of
                Nothing ->
                    False

                Just currentPlayer ->
                    currentPlayer.ip == player.ip
    in
    option [ value player.ip, selected isSelected ] [ text player.name ]


renderNullPlayerOption : Html Msg
renderNullPlayerOption =
    option [ value "no_player_selected", selected True, disabled True ] [ text "Please select a player" ]


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
    div [ Styles.albumGrid ] (List.map renderAlbum albums)


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


loadPlayers : Cmd Msg
loadPlayers =
    Http.get
        { url = "/api/players"
        , expect = Http.expectJson ReceivedPlayers playerListDecoder
        }


selectPlayer : String -> Cmd Msg
selectPlayer ip =
    Http.post
        { url = "/api/select_player"
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "ip", JE.string ip )
                    ]
                )
        , expect = Http.expectWhatever (SelectedPlayer ip)
        }


loadAlbums : Cmd Msg
loadAlbums =
    Http.get
        { url = "/api/albums"
        , expect = Http.expectJson ReceivedAlbums albumDecoder
        }


loadStatus : Cmd Msg
loadStatus =
    Cmd.map StatusBarMsg StatusBar.load


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


playerListDecoder : JD.Decoder (List Player)
playerListDecoder =
    JD.list
        (JD.map2 Player
            (JD.field "ip" JD.string)
            (JD.field "name" JD.string)
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 5000 TriggerRetrieveStatus
        , Time.every 1000 TriggerUpdateClock
        ]
