module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import FontAwesome.Styles
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (disabled, href, selected, src, value)
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
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { url : Url.Url
    , key : Nav.Key
    , playerList : PlayersListModel
    , player : Maybe Player
    , albumList : AlbumListModel
    , currentAlbum : Maybe Album
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


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { url = url
      , key = key
      , playerList = PlayersLoading
      , player = Nothing
      , albumList = Loading
      , currentAlbum = Nothing
      , status = StatusBar.init
      }
      -- Players are only loaded once to fix strange results
      -- when the player selector dropdown changed by removing
      -- the 'no player selected' option.
    , loadPlayers
    )


fireInitCmds : Maybe Player -> Cmd Msg
fireInitCmds maybePlayer =
    case maybePlayer of
        Nothing ->
            Cmd.none

        Just player ->
            Cmd.batch [ loadAlbums player, loadStatus player ]



-- UPDATE


type Msg
    = ReceivedPlayers (Result Http.Error (List Player))
    | PlayerChosen String
    | SelectedPlayer String (Result Http.Error ())
    | ReceivedAlbums (Result Http.Error (List Album))
    | AlbumChosen Player Album
    | StartedAlbum Player Album (Result Http.Error ())
    | StatusBarMsg StatusBar.Msg
    | TriggerRetrieveStatus (Maybe Player) Time.Posix
    | TriggerUpdateClock (Maybe Player) Time.Posix
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedPlayers result ->
            case result of
                Err _ ->
                    ( { model | playerList = PlayersError }, Cmd.none )

                Ok players ->
                    let
                        player =
                            playerFromUrl players model.url
                    in
                    ( { model | playerList = Players players, player = player }, fireInitCmds player )

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
                    ( model, fireInitCmds Nothing )

                Ok _ ->
                    let
                        player =
                            findPlayerByIp model ip
                    in
                    ( { model | status = replacePlayerInStatus model ip }, fireInitCmds player )

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

        TriggerRetrieveStatus maybePlayer time ->
            let
                cmd =
                    case maybePlayer of
                        Nothing ->
                            Cmd.none

                        Just player ->
                            Cmd.map StatusBarMsg (StatusBar.loadPlaybackState player)
            in
            ( model, cmd )

        TriggerUpdateClock maybePlayer time ->
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

        UrlChanged url ->
            let
                player =
                    case model.playerList of
                        Players players ->
                            playerFromUrl players url

                        _ ->
                            Nothing
            in
            ( { model | player = player }, fireInitCmds player )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )


replacePlayerInStatus : Model -> String -> StatusBar.Model
replacePlayerInStatus model ip =
    let
        playbackState =
            model.status.playbackState

        newPlaybackState =
            { playbackState | player = findPlayerByIp model ip }

        status =
            model.status
    in
    { status | playbackState = newPlaybackState }


findPlayerByIp : Model -> String -> Maybe Player
findPlayerByIp model ip =
    let
        players =
            case model.playerList of
                Players p ->
                    p

                _ ->
                    []
    in
    List.filter (\p -> p.ip == ip) players |> List.head


findPlayerByNameCaseInsensitive : Model -> String -> Maybe Player
findPlayerByNameCaseInsensitive model name =
    let
        lowerQuery =
            String.toLower name
    in
    findPlayerByFn model (\p -> String.toLower (Debug.log "Name" p.name) == lowerQuery)


findPlayerByFn : Model -> (Player -> Bool) -> Maybe Player
findPlayerByFn model fn =
    let
        players =
            case model.playerList of
                Players p ->
                    p

                _ ->
                    []
    in
    List.filter fn players |> List.head


isPaused : Model -> Bool
isPaused model =
    model.status.playbackState.state == Paused



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Music"
    , body =
        [ renderPage model |> toUnstyled ]
    }


renderPage : Model -> Html Msg
renderPage model =
    let
        playerDisplay =
            case model.player of
                Nothing ->
                    [ div [] [ text "First select a player" ]
                    ]

                Just player ->
                    [ renderAlbumList player model.albumList
                    , renderStatusBar player model.status
                    ]
    in
    div [ Styles.body ]
        ([ HS.fromUnstyled FontAwesome.Styles.css
         , renderPlayerSelector model
         ]
            ++ playerDisplay
        )


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
                    model.player
            in
            ul [ Styles.playerSelector ]
                (List.map
                    (renderPlayerOption maybeCurrentPlayer)
                    players
                )


renderPlayerOption : Maybe Player -> Player -> Html Msg
renderPlayerOption maybeCurrentPlayer player =
    let
        isSelected =
            case maybeCurrentPlayer of
                Nothing ->
                    False

                Just currentPlayer ->
                    currentPlayer.ip == player.ip

        selectedStyle =
            if isSelected then
                [ Styles.selectedPlayer ]

            else
                []

        link =
            "#" ++ player.name
    in
    li [ Styles.player ]
        [ a ([ Styles.playerLink, href link ] ++ selectedStyle) [ text player.name ]
        ]


renderAlbumList : Player -> AlbumListModel -> Html Msg
renderAlbumList player albumListModel =
    case albumListModel of
        Loading ->
            div [] [ text "Loading" ]

        Albums albums ->
            renderAlbumGrid player albums

        Error ->
            div [] [ text "Error" ]


renderAlbumGrid : Player -> List Album -> Html Msg
renderAlbumGrid player albums =
    div [ Styles.albumGrid ] (List.map (renderAlbum player) (List.take 30 albums))


renderAlbum : Player -> Album -> Html Msg
renderAlbum player album =
    -- This is a workaround for the current forwarding situation in Elm dev mode
    let
        fullPath =
            concat [ "/api", album.coverPath ]
    in
    li [ Styles.album, Styles.coverImage fullPath, onDoubleClick (AlbumChosen player album) ]
        [ div [ Styles.description ]
            [ div [ Styles.title ] [ text album.title ]
            , div [ Styles.artist ] [ text album.artist ]
            ]
        ]


renderStatusBar : Player -> StatusBar.Model -> Html Msg
renderStatusBar player status =
    HS.map StatusBarMsg (StatusBar.view player status)


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


loadAlbums : Player -> Cmd Msg
loadAlbums player =
    Http.get
        { url = buildPlayerUrl "albums" player
        , expect = Http.expectJson ReceivedAlbums albumDecoder
        }


buildPlayerUrl : String -> Player -> String
buildPlayerUrl string player =
    let
        playerPart =
            "/" ++ String.toLower player.name
    in
    String.concat [ "/api/", playerPart, "/", string ]


loadStatus : Player -> Cmd Msg
loadStatus player =
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
        [ Time.every 5000 (TriggerRetrieveStatus model.player)
        , Time.every 1000 (TriggerUpdateClock model.player)
        ]


playerFromUrl : List Player -> Url.Url -> Maybe Player
playerFromUrl playerList url =
    let
        fragment =
            url.fragment |> Maybe.withDefault ""
    in
    List.filter (\el -> fragment == el.name) playerList |> List.head
