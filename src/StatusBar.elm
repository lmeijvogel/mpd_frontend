module StatusBar exposing (..)

import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Solid as Icon
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (src)
import Html.Styled.Events exposing (onClick, onDoubleClick)
import Http exposing (jsonBody)
import Json.Decode as JD exposing (Decoder, bool, decodeString, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as JE
import StatusBarStyles
import String exposing (toLower)
import Time



-- MODEL


type alias Model =
    { playlist : List PlaylistEntry
    , playbackState : PlaybackState
    , showPanel : Bool
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
    , player : Maybe Player
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


type alias Player =
    { ip : String
    , name : String
    }


init : Model
init =
    { playlist = []
    , playbackState = initPlaybackState
    , showPanel = True
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
    , player = Nothing
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
        |> required "player" decodePlayer
        |> optional "songid" (JD.maybe int) Nothing
        |> optional "elapsed" (JD.maybe float) Nothing
        |> optional "duration" (JD.maybe float) Nothing


decodePlayer : Decoder (Maybe Player)
decodePlayer =
    JD.maybe <|
        JD.map2 Player
            (JD.field "ip" string)
            (JD.field "name" string)


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
    | ChangedPlaybackSetting PlaybackSetting Bool
    | StoredPlaybackSetting PlaybackSetting Bool (Result Http.Error ())
    | ClickedPlayerCommand PlayerCommand
    | PlaylistEntryClicked PlaylistEntry
    | SentPlayerCommand (Result Http.Error ())
    | ShowHideIconClicked Bool


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
                    ( { model | playlist = [] }, Cmd.none )

                Ok entries ->
                    ( { model | playlist = entries }, Cmd.none )

        ChangedPlaybackSetting setting value ->
            ( model, storePlaybackSetting setting value )

        StoredPlaybackSetting setting value result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok _ ->
                    ( { model | playbackState = updateSetting model.playbackState setting value }, Cmd.none )

        ClickedPlayerCommand command ->
            ( model, sendPlayerCommand command )

        PlaylistEntryClicked playlistEntry ->
            ( model, startSong playlistEntry )

        SentPlayerCommand command ->
            ( model, loadPlaybackState )

        ShowHideIconClicked newState ->
            ( { model | showPanel = newState }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ StatusBarStyles.panel ]
        [ renderTopBar model
        , div [ StatusBarStyles.mainContents model.showPanel ]
            [ renderPlaylist model
            , div []
                (List.map (renderCheckbox model)
                    [ Repeat, Single, Random, Consume ]
                )
            ]
        ]


renderTopBar : Model -> Html Msg
renderTopBar model =
    div [ StatusBarStyles.topBar ]
        [ renderPlayerButtons model.playbackState.state
        , renderCurrentSong model
        , renderSongProgress model
        , renderShowHideButton model.showPanel
        ]


renderCurrentSong : Model -> Html Msg
renderCurrentSong model =
    case model.playbackState.songId of
        Nothing ->
            div [] []

        Just songId ->
            let
                currentSong =
                    List.filter (\item -> item.id == songId) model.playlist |> List.head

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


renderPlayerButtons : PlayerState -> Html Msg
renderPlayerButtons state =
    div []
        [ renderButton Previous Icon.stepBackward False
        , renderButton Play Icon.play (state == Playing)
        , renderButton Pause Icon.pause (state == Paused)
        , renderButton Stop Icon.stop (state == Stopped)
        , renderButton Next Icon.stepForward False
        ]


renderButton : PlayerCommand -> Icon.Icon -> Bool -> Html Msg
renderButton command icon isActive =
    let
        isActiveStyling =
            if isActive then
                [ StatusBarStyles.activeButton ]

            else
                []
    in
    HS.span ([ StatusBarStyles.controlButton, onClick (ClickedPlayerCommand command) ] ++ isActiveStyling)
        [ HS.fromUnstyled (Icon.viewIcon icon) ]


renderShowHideButton : Bool -> Html Msg
renderShowHideButton currentlyShowing =
    let
        icon =
            if currentlyShowing then
                Icon.angleDoubleDown

            else
                Icon.angleDoubleUp
    in
    div [ onClick (ShowHideIconClicked (not currentlyShowing)) ] [ HS.fromUnstyled (Icon.viewIcon icon) ]


renderPlaylist : Model -> Html Msg
renderPlaylist model =
    ul [ StatusBarStyles.playlist ] (List.map (renderPlaylistEntry model.playbackState.songId) model.playlist)


renderPlaylistEntry : Maybe Int -> PlaylistEntry -> Html Msg
renderPlaylistEntry currentSongId entry =
    let
        clickHandler =
            onDoubleClick (PlaylistEntryClicked entry)

        isSongSelected =
            case currentSongId of
                Nothing ->
                    False

                Just songId ->
                    songId == entry.id

        elementClass =
            if isSongSelected then
                [ clickHandler, StatusBarStyles.selected ]

            else
                [ clickHandler ]
    in
    li elementClass [ text entry.title ]


renderCheckbox : Model -> PlaybackSetting -> Html Msg
renderCheckbox model setting =
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
            [ onClick (ChangedPlaybackSetting setting (not isActive)) ]
    in
    span (class ++ clickHandler) [ HS.fromUnstyled (Icon.viewIcon (getIconForSetting setting)) ]


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


storePlaybackSetting : PlaybackSetting -> Bool -> Cmd Msg
storePlaybackSetting setting value =
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
        { url = "/api/update_playback_setting"
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "key", JE.string requestSettingName )
                    , ( "value", JE.string requestValue )
                    ]
                )
        , expect = Http.expectWhatever (StoredPlaybackSetting setting value)
        }


sendPlayerCommand : PlayerCommand -> Cmd Msg
sendPlayerCommand command =
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
        { url = "api/command"
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "command", JE.string commandString ) ]
                )
        , expect = Http.expectWhatever SentPlayerCommand
        }


startSong : PlaylistEntry -> Cmd Msg
startSong playlistEntry =
    Http.post
        { url = "api/play_id"
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "id", JE.int playlistEntry.id ) ]
                )
        , expect = Http.expectWhatever SentPlayerCommand
        }


loadPlaylist : Cmd Msg
loadPlaylist =
    Http.get
        { url = "/api/playlist"
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


load : Cmd Msg
load =
    Cmd.batch
        [ loadPlaylist
        , loadPlaybackState
        ]


loadPlaybackState : Cmd Msg
loadPlaybackState =
    Http.get
        { url = "/api/status"
        , expect = Http.expectJson ReceivedStatus decodePlaybackState
        }
