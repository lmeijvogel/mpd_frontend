module StatusPage exposing (..)

import FontAwesome.Icon as Icon
import FontAwesome.Solid as SolidIcon
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (src)
import Html.Styled.Events exposing (onClick, onDoubleClick, stopPropagationOn)
import Http exposing (jsonBody)
import Json.Decode as JD exposing (Decoder, bool, float, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as JE
import Player exposing (Player)
import Responsive
import SingleSlider
import StatusPageStyles
import String exposing (concat, toLower)



-- MODEL


type alias Model =
    { playlist : Maybe (List PlaylistEntry)
    , playbackState : PlaybackState
    , secondsSinceLastUpdate : Int
    }


type alias PlaylistEntry =
    { id : Int
    , artist : String
    , title : String
    , position : Int
    , time : Int
    }


type alias PlaybackState =
    { repeat : Bool
    , random : Bool
    , single : Bool
    , consume : Bool
    , state : PlayerState
    , volume : Maybe Int
    , outputs : List Output
    , songId : Maybe Int
    , elapsed : Maybe Float
    , duration : Maybe Float
    , volumeSlider : Maybe (SingleSlider.SingleSlider Msg)
    , currentAlbumCover : Maybe String
    }


type PlayerCommand
    = Previous
    | Play
    | Pause
    | Stop
    | Next


type PlayerState
    = Playing
    | Paused
    | Stopped


type PlaybackSetting
    = Random
    | Single
    | Repeat
    | Consume


type alias Output =
    { id : Int
    , name : String
    , isEnabled : Bool
    }


init : Model
init =
    { playlist = Nothing
    , playbackState = initPlaybackState
    , secondsSinceLastUpdate = 0
    }


initPlaybackState : PlaybackState
initPlaybackState =
    { repeat = False
    , random = False
    , single = False
    , consume = False
    , state = Stopped
    , volume = Nothing
    , outputs = []
    , songId = Nothing
    , elapsed = Nothing
    , duration = Nothing
    , volumeSlider = Nothing
    , currentAlbumCover = Nothing
    }


updateSetting : PlaybackState -> PlaybackSetting -> Bool -> PlaybackState
updateSetting playbackState setting value =
    case setting of
        Random ->
            { playbackState | random = value }

        Repeat ->
            { playbackState | repeat = value }

        Single ->
            { playbackState | single = value }

        Consume ->
            { playbackState | consume = value }


decodePlaybackState : JD.Decoder PlaybackState
decodePlaybackState =
    JD.succeed PlaybackState
        |> required "repeat" bool
        |> required "random" bool
        |> required "single" bool
        |> required "consume" bool
        |> required "state" decodeState
        |> optional "volume" (JD.maybe int) Nothing
        |> required "outputs" (list decodeOutput)
        |> optional "songid" (JD.maybe int) Nothing
        |> optional "elapsed" (JD.maybe float) Nothing
        |> optional "duration" (JD.maybe float) Nothing
        |> hardcoded Nothing
        |> optional "album_cover_path" (JD.maybe string) Nothing


decodeOutput : Decoder Output
decodeOutput =
    JD.map3 Output
        (JD.field "id" int)
        (JD.field "name" string)
        (JD.field "is_enabled" bool)


decodeState : Decoder PlayerState
decodeState =
    string |> JD.map stringStateToPlayerState


stringStateToPlayerState : String -> PlayerState
stringStateToPlayerState input =
    case input of
        "play" ->
            Playing

        "pause" ->
            Paused

        _ ->
            Stopped



-- UPDATE


type Msg
    = ReceivedStatus Player (Result Http.Error PlaybackState)
    | ReceivedPlaylist (Result Http.Error (List PlaylistEntry))
    | ChangedPlaybackSetting Player PlaybackSetting Bool
    | StoredPlaybackSetting Player PlaybackSetting Bool (Result Http.Error ())
    | ClickedPlayerCommand Player PlayerCommand
    | PlaylistEntryClicked Player PlaylistEntry
    | SentPlayerCommand Player (Result Http.Error ())
    | OutputClicked Player Output
    | OutputActivated Player Output (Result Http.Error ())
    | VolumeChanged Player Float
    | StoredVolume Player Int (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ReceivedStatus player result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok status ->
                    let
                        statusWithVolumeSlider =
                            addVolumeSlider player status
                    in
                    ( { model | playbackState = statusWithVolumeSlider, secondsSinceLastUpdate = 0 }, Cmd.none )

        ReceivedPlaylist result ->
            case result of
                Err _ ->
                    ( { model | playlist = Nothing }, Cmd.none )

                Ok entries ->
                    ( { model | playlist = Just entries }, Cmd.none )

        ChangedPlaybackSetting player setting value ->
            ( model, storePlaybackSetting player setting value )

        StoredPlaybackSetting player setting value result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok _ ->
                    ( { model | playbackState = updateSetting model.playbackState setting value }, Cmd.none )

        ClickedPlayerCommand player command ->
            ( model, sendPlayerCommand player command )

        PlaylistEntryClicked player playlistEntry ->
            ( model, startSong player playlistEntry )

        SentPlayerCommand player command ->
            ( model, loadPlaybackState player )

        OutputClicked player output ->
            ( model, activateOutput player output )

        OutputActivated player output result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok _ ->
                    ( model, loadPlaybackState player )

        VolumeChanged player volume ->
            let
                existingPlaybackState =
                    model.playbackState

                newSlider =
                    Maybe.map (SingleSlider.update volume) existingPlaybackState.volumeSlider

                newPlaybackState =
                    { existingPlaybackState | volume = Just (floor volume), volumeSlider = newSlider }
            in
            ( { model | playbackState = newPlaybackState }, setVolume player (floor volume) )

        StoredVolume _ _ _ ->
            ( model, Cmd.none )


addVolumeSlider : Player -> PlaybackState -> PlaybackState
addVolumeSlider player state =
    let
        initVolumeSlider p volume =
            SingleSlider.init
                { min = 0
                , max = 100
                , step = 1
                , value = volume
                , onChange = VolumeChanged p
                }
    in
    { state | volumeSlider = Maybe.map (\volume -> initVolumeSlider player (toFloat volume)) state.volume }



-- VIEW


view : Player -> Model -> Responsive.ClientType -> Html Msg
view player model clientType =
        div [ StatusPageStyles.desktopMainContents ]
            [ renderPlaylist player model
            , div [ StatusPageStyles.bigAlbumCover ]
                [ renderBigAlbumCover model.playbackState.currentAlbumCover ]
            , div [ StatusPageStyles.controls ]
                [ renderPlayModeButtons player model
                , renderOutputs player model.playbackState
                , renderVolumeSlider player model.playbackState
                ]
            ]


renderBigAlbumCover : Maybe String -> Html Msg
renderBigAlbumCover maybeAlbumCover =
    case maybeAlbumCover of
        Nothing ->
            div [] [ text "No current album" ]

        Just albumCover ->
            let
                albumPath =
                    String.concat
                        [ "/api/covers/"
                        , albumCover
                        ]
            in
            img [ StatusPageStyles.bigAlbumCover, src albumPath ] []


renderPlayModeButtons : Player -> Model -> Html Msg
renderPlayModeButtons player model =
    div []
        (List.map (renderCheckbox player model)
            [ Repeat, Single, Random, Consume ]
        )


renderVolumeSlider : Player -> PlaybackState -> Html Msg
renderVolumeSlider player state =
    case state.volumeSlider of
        Nothing ->
            span [] []

        Just volumeSlider ->
            div []
                [ HS.fromUnstyled (SingleSlider.view volumeSlider)
                ]


renderStatusSummary : Player -> Model -> Responsive.ClientType -> Html Msg
renderStatusSummary player model clientType =
    let
        playerButtons =
            case clientType of
                Responsive.Desktop ->
                    renderPlayerButtons

                Responsive.Mobile ->
                    renderMinimalPlayerButtons

                Responsive.Unknown ->
                    renderPlayerButtons
    in
    div
        [ StatusPageStyles.statusSummary ]
        [ playerButtons player model.playbackState.state
        , renderCurrentSong model
        , renderSongProgress model
        , renderShowStatusButton
        ]


renderCurrentSong : Model -> Html Msg
renderCurrentSong model =
    let
        title =
            case currentSong model of
                Nothing ->
                    ""

                Just song ->
                    song.artist ++ " - " ++ song.title
    in
    div [] [ text title ]


currentSong : Model -> Maybe PlaylistEntry
currentSong model =
    case ( model.playbackState.songId, model.playlist ) of
        ( Just songId, Just playlist ) ->
            getSongFromPlaylist songId playlist

        _ ->
            Nothing


getSongFromPlaylist : Int -> List PlaylistEntry -> Maybe PlaylistEntry
getSongFromPlaylist songId playlist =
    List.filter (\item -> item.id == songId) playlist |> List.head


renderSongProgress : Model -> Html Msg
renderSongProgress model =
    let
        formattedValue =
            case model.playbackState.duration of
                Just duration ->
                    case model.playbackState.elapsed of
                        Just elapsed ->
                            formatCurrentTimeInSong (elapsed + toFloat model.secondsSinceLastUpdate) duration

                        Nothing ->
                            formatSongDuration duration

                Nothing ->
                    ""
    in
    div [] [ text formattedValue ]


formatCurrentTimeInSong : Float -> Float -> String
formatCurrentTimeInSong elapsed duration =
    formatAsTime elapsed ++ "/" ++ formatAsTime duration


formatSongDuration : Float -> String
formatSongDuration =
    formatAsTime


formatAsTime : Float -> String
formatAsTime seconds =
    let
        intSeconds =
            floor seconds

        minutes =
            intSeconds // 60 |> String.fromInt

        onlySeconds =
            modBy 60 intSeconds |> String.fromInt |> String.padLeft 2 '0'
    in
    minutes ++ ":" ++ onlySeconds


renderMinimalPlayerButtons : Player -> PlayerState -> Html Msg
renderMinimalPlayerButtons player state =
    let
        showPlayButton =
            state /= Playing

        playPauseButton =
            if showPlayButton then
                renderButton player Play SolidIcon.play (state == Playing)

            else
                renderButton player Pause SolidIcon.pause (state == Paused)
    in
    div []
        [ renderButton player Previous SolidIcon.stepBackward False
        , playPauseButton
        , renderButton player Next SolidIcon.stepForward False
        ]


renderPlayerButtons : Player -> PlayerState -> Html Msg
renderPlayerButtons player state =
    div [ StatusPageStyles.controlButtons ]
        [ renderButton player Previous SolidIcon.stepBackward False
        , renderButton player Play SolidIcon.play (state == Playing)
        , renderButton player Pause SolidIcon.pause (state == Paused)
        , renderButton player Stop SolidIcon.stop (state == Stopped)
        , renderButton player Next SolidIcon.stepForward False
        ]


renderButton : Player -> PlayerCommand -> Icon.Icon -> Bool -> Html Msg
renderButton player command icon isActive =
    -- These buttons need stopPropagation because we don't want
    -- to interpret clicks on them as a request to open the status
    -- page.
    let
        onClickWithStopPropagation : msg -> Attribute msg
        onClickWithStopPropagation message =
            stopPropagationOn "click" (JD.map alwaysStopPropagation (JD.succeed message))

        alwaysStopPropagation : msg -> ( msg, Bool )
        alwaysStopPropagation msg =
            ( msg, True )

        isActiveStyling =
            if isActive then
                [ StatusPageStyles.activeButton ]

            else
                []
    in
    HS.span ([ StatusPageStyles.controlButton, onClickWithStopPropagation (ClickedPlayerCommand player command) ] ++ isActiveStyling)
        [ HS.fromUnstyled (Icon.viewIcon icon) ]



-- The showStatus button just has a signaling value, clicking anywhere in the bar will
-- load the status display.


renderShowStatusButton : Html Msg
renderShowStatusButton =
    div [ StatusPageStyles.showHideButton ] [ HS.fromUnstyled (Icon.viewIcon SolidIcon.angleDoubleUp) ]


renderPlaylist : Player -> Model -> Html Msg
renderPlaylist player model =
    case model.playlist of
        Nothing ->
            div [] [ text "Loading playlist" ]

        Just items ->
            div [ StatusPageStyles.playlistContainer ]
                [ ol [ StatusPageStyles.playlist ] (List.map (renderPlaylistEntry model.playbackState.songId player) items)
                ]


renderPlaylistEntry : Maybe Int -> Player -> PlaylistEntry -> Html Msg
renderPlaylistEntry currentSongId player entry =
    let
        clickHandler =
            onDoubleClick (PlaylistEntryClicked player entry)

        isSongSelected =
            case currentSongId of
                Nothing ->
                    False

                Just songId ->
                    songId == entry.id

        elementClass =
            if isSongSelected then
                [ clickHandler, StatusPageStyles.playlistEntry, StatusPageStyles.selected ]

            else
                [ clickHandler, StatusPageStyles.playlistEntry ]
    in
    li elementClass
        [ div [ StatusPageStyles.playlistTitle ] [ text entry.title ]
        , div [ StatusPageStyles.playlistArtist ] [ text entry.artist ]
        ]


renderCheckbox : Player -> Model -> PlaybackSetting -> Html Msg
renderCheckbox player model setting =
    let
        isActive =
            getValueFromStatus model.playbackState setting

        class : List (Attribute Msg)
        class =
            if isActive then
                [ StatusPageStyles.playModeButton, StatusPageStyles.activeButton ]

            else
                [ StatusPageStyles.playModeButton, StatusPageStyles.inactiveButton ]

        clickHandler : List (Attribute Msg)
        clickHandler =
            [ onClick (ChangedPlaybackSetting player setting (not isActive)) ]
    in
    span (class ++ clickHandler) [ div [] [ text (getNameFromSetting setting) ] ]


renderOutputs : Player -> PlaybackState -> Html Msg
renderOutputs player playbackState =
    ul [] (List.map (renderOutput player) playbackState.outputs)


renderOutput : Player -> Output -> Html Msg
renderOutput player output =
    let
        style =
            case output.isEnabled of
                True ->
                    StatusPageStyles.activeOutput

                False ->
                    StatusPageStyles.inactiveOutput
    in
    li [ StatusPageStyles.outputRow ]
        [ button [ onClick (OutputClicked player output), StatusPageStyles.output, style ]
            [ text output.name ]
        ]


activateOutput : Player -> Output -> Cmd Msg
activateOutput player output =
    Http.post
        { url = buildPlayerUrl "enable_output" player
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "id", JE.int output.id )
                    ]
                )
        , expect = Http.expectWhatever (OutputActivated player output)
        }


getNameFromSetting : PlaybackSetting -> String
getNameFromSetting setting =
    case setting of
        Repeat ->
            "Repeat"

        Single ->
            "Single"

        Random ->
            "Random"

        Consume ->
            "Consume"


getValueFromStatus : PlaybackState -> PlaybackSetting -> Bool
getValueFromStatus state setting =
    case setting of
        Repeat ->
            state.repeat

        Single ->
            state.single

        Random ->
            state.random

        Consume ->
            state.consume


storePlaybackSetting : Player -> PlaybackSetting -> Bool -> Cmd Msg
storePlaybackSetting player setting value =
    let
        requestSettingName =
            getNameFromSetting setting |> toLower

        requestValue =
            case value of
                True ->
                    "1"

                False ->
                    "0"
    in
    Http.post
        { url = buildPlayerUrl "update_playback_setting" player
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "key", JE.string requestSettingName )
                    , ( "value", JE.string requestValue )
                    ]
                )
        , expect = Http.expectWhatever (StoredPlaybackSetting player setting value)
        }


sendPlayerCommand : Player -> PlayerCommand -> Cmd Msg
sendPlayerCommand player command =
    let
        commandString =
            case command of
                Previous ->
                    "previous"

                Play ->
                    "play"

                Pause ->
                    "pause"

                Stop ->
                    "stop"

                Next ->
                    "next"
    in
    Http.post
        { url = buildPlayerUrl "command" player
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "command", JE.string commandString ) ]
                )
        , expect = Http.expectWhatever (SentPlayerCommand player)
        }


startSong : Player -> PlaylistEntry -> Cmd Msg
startSong player playlistEntry =
    Http.post
        { url = buildPlayerUrl "play_id" player
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "id", JE.int playlistEntry.id ) ]
                )
        , expect = Http.expectWhatever (SentPlayerCommand player)
        }


setVolume : Player -> Int -> Cmd Msg
setVolume player volume =
    Http.post
        { url = buildPlayerUrl "volume" player
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "volume", JE.int volume ) ]
                )
        , expect = Http.expectWhatever (StoredVolume player volume)
        }


loadPlaylist : Player -> Cmd Msg
loadPlaylist player =
    Http.get
        { url = buildPlayerUrl "playlist" player
        , expect = Http.expectJson ReceivedPlaylist playlistDecoder
        }


playlistDecoder : JD.Decoder (List PlaylistEntry)
playlistDecoder =
    JD.list
        (JD.map5 PlaylistEntry
            (JD.field "id" JD.int)
            (JD.field "artist" JD.string)
            (JD.field "title" JD.string)
            (JD.field "position" JD.int)
            (JD.field "time" JD.int)
        )


load : Player -> Cmd Msg
load player =
    Cmd.batch
        [ loadPlaylist player
        , loadPlaybackState player
        ]


loadPlaybackState : Player -> Cmd Msg
loadPlaybackState player =
    Http.get
        { url = buildPlayerUrl "status" player
        , expect = Http.expectJson (ReceivedStatus player) decodePlaybackState
        }


buildPlayerUrl : String -> Player -> String
buildPlayerUrl string player =
    let
        playerPart =
            String.toLower player.name
    in
    String.concat [ "/api/", playerPart, "/", string ]
