module Main exposing (Model, Msg, Tile, main)

import Browser
import Dict exposing (Dict)
import Engine.Point exposing (Point)
import Engine.Render as Render
import Html exposing (Html, main_)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes



-- TILE


type Tile
    = Water
    | Grass



-- MODEL


type alias Model =
    { tiles : Dict Point Tile }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Dict.fromList
            [ ( ( 0, 0 ), Grass )
            , ( ( 1, -1 ), Water )
            , ( ( 0, 1 ), Water )
            ]
        )
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


viewTile : ( Point, Tile ) -> Svg msg
viewTile ( position, _ ) =
    Render.viewHex [ Render.hexTransform position ]


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
