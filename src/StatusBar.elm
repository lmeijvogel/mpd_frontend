module StatusBar exposing (..)

import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (src)
import Html.Styled.Events exposing (onClick)
import Http exposing (jsonBody)
import Json.Decode as JD exposing (Decoder, bool, decodeString, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as JE
import String exposing (toLower)



-- MODEL


type alias Model =
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


type PlayerState
    = Playing
    | Stopped


type PlaylistSetting
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


updateSetting : Model -> PlaylistSetting -> Bool -> Model
updateSetting model setting value =
    case setting of
        Random ->
            { model | random = value }

        Repeat ->
            { model | repeat = value }

        Single ->
            { model | single = value }

        Consume ->
            { model | consume = value }


decodeStatus : JD.Decoder Model
decodeStatus =
    JD.succeed Model
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
            (JD.field "name" string)
            (JD.field "ip" string)


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

        _ ->
            Stopped



-- UPDATE


type Msg
    = ReceivedStatus (Result Http.Error Model)
    | ChangedPlaylistSetting PlaylistSetting Bool
    | StoredPlaylistSetting PlaylistSetting Bool (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ReceivedStatus result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok newStatus ->
                    ( newStatus, Cmd.none )

        ChangedPlaylistSetting setting value ->
            ( model, storePlaylistSetting setting value )

        StoredPlaylistSetting setting value result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok _ ->
                    ( updateSetting model setting value, Cmd.none )



-- VIEW


view : Model -> Html Msg
view status =
    div []
        (List.map (renderCheckbox status)
            [ Repeat, Single, Random, Consume ]
        )


renderCheckbox : Model -> PlaylistSetting -> Html Msg
renderCheckbox status setting =
    span [] [ HS.input [ Html.Styled.Attributes.type_ "checkbox", Html.Styled.Attributes.checked (getValueFromStatus status setting), Html.Styled.Events.onCheck (ChangedPlaylistSetting setting) ] [], span [] [ text (getNameFromSetting setting) ] ]


getNameFromSetting : PlaylistSetting -> String
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


getValueFromStatus : Model -> PlaylistSetting -> Bool
getValueFromStatus status setting =
    case setting of
        Repeat ->
            status.repeat

        Single ->
            status.single

        Random ->
            status.random

        Consume ->
            status.consume


storePlaylistSetting : PlaylistSetting -> Bool -> Cmd Msg
storePlaylistSetting setting value =
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
        , expect = Http.expectWhatever (StoredPlaylistSetting setting value)
        }


load : Cmd Msg
load =
    Http.get
        { url = "/api/status"
        , expect = Http.expectJson ReceivedStatus decodeStatus
        }
