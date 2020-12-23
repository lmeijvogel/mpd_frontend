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
import PlayerDisplay exposing (..)
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
    , playerModel : Maybe PlayerDisplay.Model
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
      , playerModel = Nothing
      }
      -- Players are only loaded once to fix strange results
      -- when the player selector dropdown changed by removing
      -- the 'no player selected' option.
    , loadPlayers
    )


loadPlayerStatus : PlayerDisplay.Model -> Cmd Msg
loadPlayerStatus playerModel =
    Cmd.map PlayerMsg (PlayerDisplay.loadAlbumsAndStatus playerModel)



-- UPDATE


type Msg
    = ReceivedPlayers (Result Http.Error (List Player))
    | PlayerChosen String
    | SelectedPlayer String (Result Http.Error ())
    | TriggerRetrieveStatus (Maybe Player) Time.Posix
    | TriggerUpdateClock (Maybe Player) Time.Posix
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | PlayerMsg PlayerDisplay.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedPlayers result ->
            case result of
                Err _ ->
                    ( { model | playerList = PlayersError }, Cmd.none )

                Ok players ->
                    let
                        maybePlayer =
                            playerFromUrl players model.url

                        newPlayerModel =
                            Maybe.map PlayerDisplay.init maybePlayer

                        cmd =
                            Maybe.map loadPlayerStatus newPlayerModel |> Maybe.withDefault Cmd.none
                    in
                    ( { model | playerList = Players players, playerModel = newPlayerModel }, cmd )

        PlayerChosen ip ->
            let
                maybePlayer =
                    findPlayerByIp model ip

                newPlayerModel =
                    Maybe.map PlayerDisplay.init maybePlayer
            in
            ( { model | playerModel = newPlayerModel }, selectPlayer ip )

        SelectedPlayer ip result ->
            case result of
                Err _ ->
                    ( { model | playerModel = Nothing }, Cmd.none )

                -- If something goes wrong, do not do anything.
                Ok _ ->
                    let
                        cmd =
                            Maybe.map loadPlayerStatus model.playerModel |> Maybe.withDefault Cmd.none
                    in
                    ( model, cmd )

        TriggerRetrieveStatus maybePlayer time ->
            let
                cmd =
                    case maybePlayer of
                        Nothing ->
                            Cmd.none

                        Just player ->
                            Cmd.map PlayerMsg (PlayerDisplay.loadPlaybackState player)
            in
            ( model, cmd )

        TriggerUpdateClock maybePlayer time ->
            case maybePlayer of
                Nothing ->
                    ( model, Cmd.none )

                Just player ->
                    let
                        newPlayerModel =
                            Maybe.map (PlayerDisplay.triggerUpdateClock time) model.playerModel
                    in
                    ( { model | playerModel = newPlayerModel }, Cmd.none )

        UrlChanged url ->
            let
                maybePlayer =
                    case model.playerList of
                        Players players ->
                            playerFromUrl players url

                        _ ->
                            Nothing

                maybePlayerModel =
                    Maybe.map PlayerDisplay.init maybePlayer
            in
            case maybePlayerModel of
                Nothing ->
                    ( { model | playerModel = Nothing }, Cmd.none )

                Just playerModel ->
                    ( { model | playerModel = Just playerModel }, loadPlayerStatus playerModel )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        PlayerMsg message ->
            case model.playerModel of
                Nothing ->
                    ( model, Cmd.none )

                Just playerModel ->
                    let
                        ( newPlayerModel, action ) =
                            PlayerDisplay.update message playerModel
                    in
                    ( { model | playerModel = Just newPlayerModel }, Cmd.map PlayerMsg action )


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
            case model.playerModel of
                Nothing ->
                    [ div [] [ text "First select a player" ]
                    ]

                Just playerModel ->
                    [ HS.map PlayerMsg
                        (PlayerDisplay.view playerModel)
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
                    case model.playerModel of
                        Nothing ->
                            Nothing

                        Just playerModel ->
                            Just playerModel.player
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


buildPlayerUrl : String -> Player -> String
buildPlayerUrl string player =
    let
        playerPart =
            "/" ++ String.toLower player.name
    in
    String.concat [ "/api/", playerPart, "/", string ]


playerListDecoder : JD.Decoder (List Player)
playerListDecoder =
    JD.list
        (JD.map2 Player
            (JD.field "ip" JD.string)
            (JD.field "name" JD.string)
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        player =
            case model.playerModel of
                Nothing ->
                    Nothing

                Just playerModel ->
                    Just playerModel.player
    in
    Sub.batch
        [ Time.every 5000 (TriggerRetrieveStatus player)
        , Time.every 1000 (TriggerUpdateClock player)
        ]


playerFromUrl : List Player -> Url.Url -> Maybe Player
playerFromUrl playerList url =
    let
        fragment =
            url.fragment |> Maybe.withDefault ""
    in
    List.filter (\el -> fragment == el.name) playerList |> List.head
