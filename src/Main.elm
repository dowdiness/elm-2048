module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Browser.Events exposing (onKeyDown)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, class, disabled, href)
import Html.Styled.Events exposing (onClick)
import RemoteData exposing (RemoteData)
import Url exposing (Url)
import Json.Decode as Decode

---- PROGRAM ----

main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = UserClickedLink
        , onUrlChange = BrowserChangedUrl
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , route : Url.Url
    , tiles: List Int
    , pressed: String
    }

type alias Flags =
    {}

initialTiles =
    List.repeat 16 2048

-- Decoder

type Direction
    = Up
    | Right
    | Down
    | Left
    | Other String

keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)

toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowUp" ->
            Up

        "ArrowRight" ->
            Right

        "ArrowDown" ->
            Down

        "ArrowLeft" ->
            Left

        _ ->
            Other string


-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Elm 2048!"
    , body = [ body model |> toUnstyled ]
    }

body : Model -> Html Msg
body model =
    div
        []
        [ h1
            [ class "text-gray-800" ]
            [ text "Elm 2048" ]
        , h2 [] [ text model.pressed ]
        , gameBoard model
        ]

gameBoard : Model -> Html Msg
gameBoard model =
    div
        [ class "w-64 flex flex-wrap"]
        (List.map gameTile (List.map String.fromInt model.tiles))

gameTile : String -> Html Msg
gameTile number =
    div
        [ class "w-16 h-16 border flex items-center justify-center" ]
        [ p [ class "text-2xl font-bold"] [ text number ] ]


-- UPDATE


type Msg
    = -- Message naming conventions: https://youtu.be/w6OVDBqergc
    BrowserChangedUrl Url
    | UserClickedLink Browser.UrlRequest
    | UserPressedKey Direction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserChangedUrl url ->
            ( { model | route = url }
            , Cmd.none
            )

        UserClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UserPressedKey key ->
            case key of
                Up ->
                    ( { model | pressed = "Up" }
                    , Cmd.none
                    )

                Right ->
                    ( { model | pressed = "Right" }
                    , Cmd.none
                    )

                Down ->
                    ( { model | pressed = "Down" }
                    , Cmd.none
                    )

                Left ->
                    ( { model | pressed = "Left" }
                    , Cmd.none
                    )

                Other pressedKey ->
                    ( { model | pressed = pressedKey  }
                    , Cmd.none
                    )


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, route = url, tiles = initialTiles, pressed = "Start" }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map UserPressedKey keyDecoder)
        ]
