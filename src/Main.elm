module Main exposing (Model, Msg, Tile, main)

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
    , cameraPosition : ( Float, Float )
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
            ]
        )
        [ Entity ( 0, 0 ) ]
        ( 0, 0 )
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedHex Point


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedHex pos ->
            ( { model
                | entities = model.entities |> List.map (\e -> { e | position = pos })
                , cameraPosition = Render.pointToPixel pos
              }
            , Cmd.none
            )



-- VIEW


viewTile : ( Point, Tile ) -> Svg Msg
viewTile ( position, tile ) =
    Render.viewHex
        [ Render.hexTransform position
        , Svg.Events.onClick (ClickedHex position)
        , Svg.Attributes.class "tile"
        , Svg.Attributes.class (tileToString tile)
        ]


viewTiles : Dict Point Tile -> Svg Msg
viewTiles tiles =
    let
        _ =
            Debug.log "tiles" ()
    in
    Svg.g []
        (tiles
            |> Dict.toList
            |> List.map viewTile
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


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Svg.svg
            [ Svg.Attributes.viewBox "-1000 -1000 2000 2000"
            , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
            , Svg.Attributes.class "game-svg"
            ]
            [ Render.camera model.cameraPosition
                [ Svg.Attributes.class "camera" ]
                [ Svg.Lazy.lazy viewTiles model.tiles
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
