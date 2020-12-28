module StatusBar exposing (..)

import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Solid as Icon
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (src)
import Html.Styled.Events exposing (onClick, onDoubleClick, stopPropagationOn)
import Http exposing (jsonBody)
import Json.Decode as JD exposing (Decoder, bool, decodeString, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as JE
import Player exposing (Player)
import Responsive
import StatusBarStyles
import String exposing (toLower)
import Time



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
    = ReceivedStatus (Result Http.Error PlaybackState)
    | ReceivedPlaylist (Result Http.Error (List PlaylistEntry))
    | ChangedPlaybackSetting Player PlaybackSetting Bool
    | StoredPlaybackSetting Player PlaybackSetting Bool (Result Http.Error ())
    | ClickedPlayerCommand Player PlayerCommand
    | PlaylistEntryClicked Player PlaylistEntry
    | SentPlayerCommand Player (Result Http.Error ())
    | OutputClicked Player Output
    | OutputActivated Player Output (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ReceivedStatus result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok newStatus ->
                    ( { model | playbackState = newStatus, secondsSinceLastUpdate = 0 }, Cmd.none )

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



-- VIEW


view : Player -> Model -> Responsive.ClientType -> Html Msg
view player model clientType =
    div [ StatusBarStyles.mainContents ]
        [ renderPlaylist player model
        , div []
            (List.map (renderCheckbox player model)
                [ Repeat, Single, Random, Consume ]
            )
        , renderOutputs player model.playbackState
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
        [ StatusBarStyles.topBar ]
        [ playerButtons player model.playbackState.state
        , renderCurrentSong model
        , renderSongProgress model
        , renderShowStatusButton
        ]


renderCurrentSong : Model -> Html Msg
renderCurrentSong model =
    case model.playbackState.songId of
        Nothing ->
            div [] []

        Just songId ->
            let
                currentSong =
                    case model.playlist of
                        Nothing ->
                            Nothing

                        Just items ->
                            List.filter (\item -> item.id == songId) items |> List.head

                title =
                    case currentSong of
                        Nothing ->
                            ""

                        Just song ->
                            song.artist ++ " - " ++ song.title
            in
            div [] [ text title ]


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
formatSongDuration duration =
    formatAsTime duration


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
                renderButton player Play Icon.play (state == Playing)

            else
                renderButton player Pause Icon.pause (state == Paused)
    in
    div []
        [ renderButton player Previous Icon.stepBackward False
        , playPauseButton
        , renderButton player Next Icon.stepForward False
        ]


renderPlayerButtons : Player -> PlayerState -> Html Msg
renderPlayerButtons player state =
    div [ StatusBarStyles.controlButtons ]
        [ renderButton player Previous Icon.stepBackward False
        , renderButton player Play Icon.play (state == Playing)
        , renderButton player Pause Icon.pause (state == Paused)
        , renderButton player Stop Icon.stop (state == Stopped)
        , renderButton player Next Icon.stepForward False
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
                [ StatusBarStyles.activeButton ]

            else
                []
    in
    HS.span ([ StatusBarStyles.controlButton, onClickWithStopPropagation (ClickedPlayerCommand player command) ] ++ isActiveStyling)
        [ HS.fromUnstyled (Icon.viewIcon icon) ]



-- The showStatus button just has a signaling value, clicking anywhere in the bar will
-- load the status display.


renderShowStatusButton : Html Msg
renderShowStatusButton =
    div [ StatusBarStyles.showHideButton ] [ HS.fromUnstyled (Icon.viewIcon Icon.angleDoubleUp) ]


renderPlaylist : Player -> Model -> Html Msg
renderPlaylist player model =
    case model.playlist of
        Nothing ->
            div [] [ text "Loading playlist" ]

        Just items ->
            ol [ StatusBarStyles.playlist ] (List.map (renderPlaylistEntry model.playbackState.songId player) items)


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
                [ clickHandler, StatusBarStyles.playlistEntry, StatusBarStyles.selected ]

            else
                [ clickHandler, StatusBarStyles.playlistEntry ]
    in
    li elementClass
        [ div [ StatusBarStyles.playlistTitle ] [ text entry.title ]
        , div [ StatusBarStyles.playlistArtist ] [ text entry.artist ]
        ]


renderCheckbox : Player -> Model -> PlaybackSetting -> Html Msg
renderCheckbox player model setting =
    let
        isActive =
            getValueFromStatus model.playbackState setting

        class : List (Attribute Msg)
        class =
            case isActive of
                True ->
                    [ StatusBarStyles.controlButton, StatusBarStyles.activeButton ]

                False ->
                    [ StatusBarStyles.controlButton ]

        clickHandler : List (Attribute Msg)
        clickHandler =
            [ onClick (ChangedPlaybackSetting player setting (not isActive)) ]
    in
    span (class ++ clickHandler) [ HS.fromUnstyled (Icon.viewIcon (getIconForSetting setting)) ]


renderOutputs : Player -> PlaybackState -> Html Msg
renderOutputs player playbackState =
    ul [] (List.map (renderOutput player) playbackState.outputs)


renderOutput : Player -> Output -> Html Msg
renderOutput player output =
    let
        style =
            case output.isEnabled of
                True ->
                    StatusBarStyles.activeOutput

                False ->
                    StatusBarStyles.inactiveOutput
    in
    li [ StatusBarStyles.outputRow ]
        [ button [ onClick (OutputClicked player output), StatusBarStyles.output, style ]
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


getIconForSetting : PlaybackSetting -> Icon.Icon
getIconForSetting setting =
    -- HS.input [ Html.Styled.Attributes.type_ "checkbox", Html.Styled.Attributes.checked (getValueFromStatus model.playbackState setting), Html.Styled.Events.onCheck (ChangedPlaybackSetting setting) ] []
    case setting of
        Repeat ->
            Icon.retweet

        Single ->
            Icon.handPointUp

        Random ->
            Icon.random

        Consume ->
            Icon.cut


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
        , expect = Http.expectJson ReceivedStatus decodePlaybackState
        }


buildPlayerUrl : String -> Player -> String
buildPlayerUrl string player =
    let
        playerPart =
            String.toLower player.name
    in
    String.concat [ "/api/", playerPart, "/", string ]
