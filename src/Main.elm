module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (src)
import Html.Styled.Events exposing (onClick)
import Http exposing (jsonBody)
import Json.Decode as JD
import Json.Encode as JE
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


type alias Cd =
    { artist : String
    , title : String
    , coverPath : String
    }


type alias PlaylistEntry =
    { id : Int
    , artist : String
    , title : String
    , position : Int
    , time : Int
    }


type PlayerState
    = Playing
    | Stopped


type alias Model =
    { cdList : CdListModel
    , currentCd : Maybe Cd
    , playlist : List PlaylistEntry
    , state : PlayerState
    }


type CdListModel
    = Loading
    | Error
    | CDs (List Cd)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cdList = Loading
      , currentCd = Nothing
      , playlist = []
      , state = Stopped
      }
    , Cmd.batch [ loadAlbums, loadPlaylist ]
    )



-- UPDATE


type Msg
    = ReceivedAlbums (Result Http.Error (List Cd))
    | ReceivedPlaylist (Result Http.Error (List PlaylistEntry))
    | CdChosen Cd
    | StartedAlbum Cd (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedAlbums result ->
            case result of
                Err _ ->
                    ( { model | cdList = Error }, Cmd.none )

                Ok cds ->
                    ( { model | cdList = CDs cds }, Cmd.none )

        ReceivedPlaylist result ->
            case result of
                Err _ ->
                    ( { model | playlist = [] }, Cmd.none )

                Ok entries ->
                    ( { model | playlist = entries }, Cmd.none )

        CdChosen cd ->
            ( model, clearAndPlay cd )

        StartedAlbum cd result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok _ ->
                    ( { model | currentCd = Just cd, state = Playing }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ Styles.body ]
        [ div [] [ renderCdList model.cdList ]
        , renderPlaylist model.playlist
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


renderPlaylist : List PlaylistEntry -> Html Msg
renderPlaylist entries =
    div [] (List.map renderPlaylistEntry entries)


renderPlaylistEntry : PlaylistEntry -> Html Msg
renderPlaylistEntry entry =
    li [] [ text entry.title ]


loadAlbums : Cmd Msg
loadAlbums =
    Http.get
        { url = "/api/albums"
        , expect = Http.expectJson ReceivedAlbums albumDecoder
        }


loadPlaylist : Cmd Msg
loadPlaylist =
    Http.get
        { url = "/api/playlist"
        , expect = Http.expectJson ReceivedPlaylist playlistDecoder
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
