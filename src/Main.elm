module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, class, disabled, href)
import Html.Styled.Events exposing (onClick)
import RemoteData exposing (RemoteData)
import Url exposing (Url)

type alias Flags =
    {}


-- MODEL


type alias Model =
    { key : Nav.Key
    , route : Url.Url
    , tiles: List Int
    }

initialTiles =
    List.repeat 16 2048

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
        , gameBoard model
        ]

gameBoard : Model -> Html Msg
gameBoard model =
    div
        [ class "w-64 sm:w-96 flex flex-wrap"]
        (List.map gameTile (List.map String.fromInt model.tiles))

gameTile : String -> Html Msg
gameTile number =
    div
        [ class "w-16 h-16 sm:w-24 sm:h-24 border flex items-center justify-center" ]
        [ p [ class "text-2xl font-bold"] [ text number ] ]


-- UPDATE


type Msg
    = -- Message naming conventions: https://youtu.be/w6OVDBqergc
    BrowserChangedUrl Url
    | UserClickedLink Browser.UrlRequest


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


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, route = url, tiles = initialTiles }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
