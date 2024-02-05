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



-- ENTITY


type alias Entity =
    { position : Point
    }



-- TILE


type Tile
    = Water
    | Grass



-- MODEL


type alias Model =
    { tiles : Dict Point Tile
    , entities : List Entity
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Dict.fromList
            [ ( ( 0, 0 ), Grass )
            , ( ( 1, -1 ), Water )
            , ( ( 0, 1 ), Water )
            , ( ( 1, 0 ), Water )
            , ( ( 2, -2 ), Water )
            ]
        )
        [ Entity ( 0, 0 ) ]
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedHex Point


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedHex pos ->
            ( { model | entities = model.entities |> List.map (\e -> { e | position = pos }) }, Cmd.none )



-- VIEW


viewTile : ( Point, Tile ) -> Svg Msg
viewTile ( position, _ ) =
    Render.viewHex
        [ Render.hexTransform position
        , Svg.Events.onClick (ClickedHex position)
        , Svg.Attributes.class "tile"
        ]


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
            , Svg.Attributes.class "game-svg"
            ]
            [ Svg.g []
                (model.tiles
                    |> Dict.toList
                    |> List.map viewTile
                )
            , Svg.g []
                (model.entities
                    |> List.map viewEntity
                )
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
