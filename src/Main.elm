module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Browser.Events exposing (onKeyDown)
import Element exposing (Element, el, text, paragraph, row, column, alignRight, fill, width, height, px, rgb255, spacing, centerX, centerY, padding)
import Element.Input as Input
import Element.Events exposing (onClick)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Url exposing (Url)
import Json.Decode as Decode
import Random
import Array

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
    , board : Board
    }

type alias Board =
    List Row

type alias Row =
    List Cell

type Cell
    = Tile Int
    | Empty

type alias Position =
    (Int, Int)

type alias Flags =
    {}

emptyBoard : Board
emptyBoard = List.repeat 4 <| List.repeat 4 <| Empty

init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, route = url, board = emptyBoard }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown directionDecoderToEventMoveTileMsg
        ]


-- Decoder

type Direction
    = Up
    | Right
    | Down
    | Left
directionDecoderToEventMoveTileMsg : Decode.Decoder Msg
directionDecoderToEventMoveTileMsg =
    Decode.map EventMoveTile directionDecoder

directionDecoder : Decode.Decoder (Maybe Direction)
directionDecoder =
    keyDecoder
        |> Decode.andThen chooseFromDirection

keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string

chooseFromDirection : String -> Decode.Decoder (Maybe Direction)
chooseFromDirection string =
    case string of
        "ArrowUp" ->
            Decode.succeed (Just Up)

        "ArrowRight" ->
            Decode.succeed (Just Right)

        "ArrowDown" ->
            Decode.succeed (Just Down)

        "ArrowLeft" ->
            Decode.succeed (Just Left)

        _ ->
            Decode.succeed Nothing

-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Elm 2048!"
    , body = [ body model |> Element.layout [] ]
    }


body : Model -> Element Msg
body model =
    column
        [centerX, centerY, spacing 12]
        [ el [Font.size 42] <| text "Elm 2048"
        , el [Background.color (rgb255 178 245 234), padding 16] (gameBoard model.board)
        , Input.button
            [ Font.color (rgb255 255 255 255), padding 8, Background.color (rgb255 117 118 119), alignRight ]
            { onPress = Just StartGame
            , label = text "Start"
            }
        ]


gameBoard : Board -> Element msg
gameBoard board =
    column
        [ spacing 16 ]
        (List.map gameRow board)

gameRow : Row -> Element msg
gameRow model =
    row
        [ spacing 16 ]
        (List.map gameTile model)

gameTile : Cell -> Element msg
gameTile cell =
    case cell of
        Tile number ->
            column
                [ width <| px 64, height <| px 64
                , Background.color <| rgb255 79 209 197
                ]
                [ el [Font.size 32, centerX, centerY]
                    <| text
                    <| String.fromInt number ]
        Empty ->
            el
                [ width <| px 64, height <| px 64
                , Background.color (rgb255 79 209 197)
                , centerX
                , centerY
                ]
                Element.none


-- UPDATE


type Msg
    = -- Message naming conventions: https://youtu.be/w6OVDBqergc
    BrowserChangedUrl Url
    | UserClickedLink Browser.UrlRequest
    | EventMoveTile (Maybe Direction)
    | StartGame
    | AddTile (Position, Cell)


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

        EventMoveTile maybeDirection ->
            case maybeDirection of
                Just direction ->
                    ( model, Cmd.none )
                Nothing ->
                    ( model, Cmd.none)
        StartGame ->
            initBoard model
        AddTile (position, cell) ->
            ( { model
                | board = setBoard position cell model.board  }
            , Cmd.none
            )


initBoard : Model -> ( Model, Cmd Msg )
initBoard model =
    ( model
    , randomPosition model.board
        |> Maybe.map (\position -> Random.pair position randomTile)
        |> Maybe.map (Random.generate AddTile)
        |> Maybe.withDefault Cmd.none
    )

setBoard : Position -> Cell -> Board -> Board
setBoard ( x, y ) cell board =
    let
        arrayBoard = Array.fromList board
    in
        Array.get x arrayBoard
            |> Maybe.map (\oldRow -> Array.toList(Array.set y cell (Array.fromList oldRow)))
            |> Maybe.map (\newRow -> Array.set x newRow arrayBoard)
            |> Maybe.map (\newBoard -> Array.toList newBoard)
            |> Maybe.withDefault board


emptyPositionList : Board -> List Position
emptyPositionList board =
            board
                |> List.indexedMap (\i -> List.indexedMap (\j -> Tuple.pair ( i, j )))
                |> List.concat
                |> List.filterMap
                    (\( position, cell ) ->
                        case cell of
                            Tile _ ->
                                Nothing

                            Empty ->
                                Just position
                    )

randomPosition : Board -> Maybe (Random.Generator Position)
randomPosition board =
    case emptyPositionList board of
        [] ->
            Nothing

        head :: tail ->
            Just (Random.uniform head tail)


randomTile : Random.Generator Cell
randomTile =
    Random.uniform (Tile 2) [ Tile 4 ]
