module Main exposing (Model, Msg, Tile, main)

import Browser
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Engine.Path as Path exposing (Node)
import Engine.Point exposing (Point)
import Engine.Render as Render
import Html exposing (Html, main_)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Lazy
import Url exposing (Url)



-- TILE


type Tile
    = Water
    | Grass


tileToString : Tile -> String
tileToString tile =
    case tile of
        Water ->
            "water"

        Grass ->
            "grass"


canMove : Dict Point Tile -> Point -> Bool
canMove tiles tile =
    case Dict.get tile tiles of
        Just Water ->
            False

        Just Grass ->
            True

        Nothing ->
            False



-- MODEL


type alias Model =
    { tiles : Dict Point Tile
    , cameraPosition : ( Int, Int )
    , hoverTile : Point
    , viewEditor : Bool
    }


parseUrl : Url -> Bool
parseUrl url =
    case url.path of
        "/editor" ->
            True

        _ ->
            False


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url _ =
    ( Model
        (Dict.fromList
            [ ( ( 0, 0 ), Grass )
            , ( ( 1, -1 ), Grass )
            , ( ( 0, 1 ), Grass )
            , ( ( 1, 0 ), Water )
            , ( ( 2, -2 ), Water )
            , ( ( 2, -1 ), Water )
            , ( ( -1, 0 ), Grass )
            ]
        )
        ( 0, 0 )
        ( 0, 0 )
        (parseUrl url)
    , Cmd.none
    )



-- UPDATE


type Msg
    = HoverHex Point
    | ClickedGhostTile Point
    | NoOp
    | ChangedUrl Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HoverHex pos ->
            ( { model
                | hoverTile = pos
              }
            , Cmd.none
            )

        ClickedGhostTile position ->
            ( { model | tiles = model.tiles |> Dict.insert position Grass }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        ChangedUrl url ->
            ( { model | viewEditor = parseUrl url }, Cmd.none )



-- VIEW


viewTile : ( Point, Tile ) -> Svg Msg
viewTile ( position, tile ) =
    Render.viewHex
        [ Render.hexTransform position
        , Svg.Events.onMouseOver (HoverHex position)
        , Svg.Attributes.class "tile"
        , Svg.Attributes.class (tileToString tile)
        ]


viewGhostTile : Point -> Svg Msg
viewGhostTile position =
    Render.viewHex
        [ Render.hexTransform position
        , Svg.Events.onClick (ClickedGhostTile position)
        , Svg.Attributes.class "ghost-tile"
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.stroke "beige"
        , Svg.Attributes.strokeWidth "1"
        ]


viewTiles : Dict Point Tile -> Svg Msg
viewTiles tiles =
    Svg.g [ Svg.Attributes.class "tiles" ]
        (tiles
            |> Dict.toList
            |> List.map viewTile
        )


viewPathNode : ( Point, Node ) -> Svg msg
viewPathNode ( pos, node ) =
    Svg.g
        [ Render.hexTransform pos
        , Svg.Attributes.fontSize "2rem"
        , Svg.Attributes.textAnchor "middle"
        , Svg.Attributes.class "path-node"
        ]
        [ Render.viewHex
            [ Svg.Attributes.fill "#262626"
            , Svg.Attributes.fillOpacity "0.2"
            ]
        , Svg.text_
            [ Svg.Attributes.x "-40"
            , Svg.Attributes.y "-20"
            , Svg.Attributes.fill "hsl(0, 75%, 50%)"
            ]
            [ Svg.text ("g: " ++ String.fromInt node.g) ]
        , Svg.text_
            [ Svg.Attributes.x "40"
            , Svg.Attributes.y "-20"
            , Svg.Attributes.fill "hsl(300, 75%, 50%)"
            ]
            [ Svg.text ("h: " ++ String.fromInt node.h) ]
        , Svg.text_
            [ Svg.Attributes.y "30"
            , Svg.Attributes.fill "hsl(200, 75%, 50%)"
            ]
            [ Svg.text ("f: " ++ String.fromInt (Path.f node)) ]
        ]


viewPath : Dict Point Tile -> Point -> Point -> Svg msg
viewPath tiles from to =
    Svg.g []
        (Path.pathfind (canMove tiles) from to
            |> Path.toList
            |> List.map viewPathNode
        )


gooFilter : Svg msg
gooFilter =
    Svg.filter [ Svg.Attributes.id "goo-filter" ]
        [ Svg.feGaussianBlur
            [ Svg.Attributes.in_ "SourceGraphic"
            , Svg.Attributes.stdDeviation "10"
            ]
            []
        , Svg.feColorMatrix
            [ Svg.Attributes.values "1 0 0 0 0      0 1 0 0 0       0 0 1 0 0       0 0 0 20 -10"
            ]
            []
        ]


viewGame : Model -> Html Msg
viewGame model =
    Render.svg
        [ Svg.Attributes.class "game-svg" ]
        [ Svg.defs [] [ gooFilter ]
        , Render.camera model.cameraPosition
            [ Svg.Attributes.class "camera" ]
            [ Svg.Lazy.lazy viewTiles model.tiles
            , viewPath model.tiles ( 0, 0 ) model.hoverTile
            ]
        ]


viewEditor : Model -> Html Msg
viewEditor model =
    Html.div [ Html.Attributes.class "editor" ]
        [ Html.section [ Html.Attributes.class "sidebar" ] [ Html.text "editor" ]
        , Render.svg
            [ Svg.Attributes.class "game-svg" ]
            [ Svg.defs [] [ gooFilter ]
            , Render.camera model.cameraPosition
                [ Svg.Attributes.class "camera" ]
                [ Svg.g [] (List.map viewGhostTile Render.square)
                , Svg.Lazy.lazy viewTiles model.tiles
                ]
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Elm app"
    , body =
        [ main_ [ Html.Attributes.id "app" ]
            [ if model.viewEditor then
                viewEditor model

              else
                viewGame model
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = always NoOp
        , onUrlChange = ChangedUrl
        }
