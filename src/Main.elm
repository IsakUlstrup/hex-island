module Main exposing (Entity, Model, Msg, Tile, main)

import Browser
import Dict exposing (Dict)
import Engine.Point exposing (Point)
import Engine.Render as Render
import Html exposing (Html, main_)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Lazy



-- ENTITY


type alias Entity =
    { position : Point
    }



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



-- MODEL


type alias Model =
    { tiles : Dict Point Tile
    , entities : List Entity
    , cameraPosition : ( Int, Int )
    , clickedTile : Maybe Point
    , hoverTile : Maybe Point
    }


init : () -> ( Model, Cmd Msg )
init _ =
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
        [ Entity ( 0, 0 ) ]
        ( 0, 0 )
        Nothing
        Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedHex Point
    | HoverHex Point


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedHex pos ->
            ( { model
                | clickedTile = Just pos
              }
            , Cmd.none
            )

        HoverHex pos ->
            ( { model
                | hoverTile = Just pos
              }
            , Cmd.none
            )



-- VIEW


svgClassList : List ( String, Bool ) -> Svg.Attribute msg
svgClassList classes =
    classes
        |> List.filter Tuple.second
        |> List.map Tuple.first
        |> String.join " "
        |> Svg.Attributes.class


viewTile : Maybe Point -> Maybe Point -> ( Point, Tile ) -> Svg Msg
viewTile hover selected ( position, tile ) =
    let
        maybeEquals : Maybe Point -> Bool
        maybeEquals m =
            case m of
                Just p ->
                    p == position

                Nothing ->
                    False
    in
    Render.viewHex
        [ Render.hexTransform position
        , Svg.Events.onClick (ClickedHex position)
        , Svg.Events.onMouseOver (HoverHex position)
        , Svg.Attributes.class "tile"
        , Svg.Attributes.class (tileToString tile)
        , svgClassList
            [ ( "hover", maybeEquals hover )
            , ( "selected", maybeEquals selected )
            ]
        ]


viewTiles : Maybe Point -> Maybe Point -> Dict Point Tile -> Svg Msg
viewTiles hover selected tiles =
    Svg.g [ Svg.Attributes.class "tiles" ]
        (tiles
            |> Dict.toList
            |> List.map (viewTile hover selected)
        )


viewEntity : Entity -> Svg msg
viewEntity entity =
    Svg.circle
        [ Svg.Attributes.r "50"
        , Svg.Attributes.cx "0"
        , Svg.Attributes.cy "0"
        , Svg.Attributes.fill "#262626"
        , Render.hexTransform entity.position
        , Svg.Attributes.class "entity"
        ]
        []


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


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Svg.svg
            [ Svg.Attributes.viewBox "-1000 -1000 2000 2000"
            , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
            , Svg.Attributes.class "game-svg"
            ]
            [ Svg.defs [] [ gooFilter ]
            , Render.camera model.cameraPosition
                [ Svg.Attributes.class "camera" ]
                [ Svg.Lazy.lazy3 viewTiles model.hoverTile model.clickedTile model.tiles
                , Svg.g []
                    (model.entities
                        |> List.map viewEntity
                    )
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
