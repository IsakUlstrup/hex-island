module Main exposing (Model, Msg, Tile, main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Engine.Codec as Codec
import Engine.Noise as Noise
import Engine.Point as Point exposing (Point)
import Engine.Render as Render exposing (Camera)
import Entity exposing (Entity)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Decoder)
import Json.Encode
import Ports
import Random exposing (Seed)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Keyed
import Svg.Lazy



-- TILE


type alias Tile =
    Int


canMove : Dict Point Tile -> Point -> Point -> Bool
canMove tiles from to =
    case ( Dict.get from tiles, Dict.get to tiles ) of
        ( Just f, Just t ) ->
            if t == 0 then
                False

            else if f > t then
                f - t == 1

            else if t > f then
                t - f == 1

            else
                True

        _ ->
            False



-- MODEL


type alias Model =
    { tiles : Dict Point Tile
    , entities : List Entity
    , camera : Camera
    , hoverTile : Point
    , mouseDown : Bool
    , viewPathDebug : Bool
    , seed : Seed
    }


randomMap : Seed -> ( Dict Point Tile, Seed )
randomMap seed =
    let
        radius : number
        radius =
            15

        ( map, newSeed ) =
            Random.step (Noise.generateCircle radius) seed

        distanceFalloff : Point -> Float
        distanceFalloff position =
            toFloat (Point.distance ( 0, 0 ) position) / radius
    in
    ( map
        |> List.filterMap
            (\( pos, val ) ->
                let
                    adjustedVal : Float
                    adjustedVal =
                        ((val - distanceFalloff pos) + 1) / 2
                in
                if adjustedVal < 0 then
                    Nothing

                else
                    Just
                        ( pos
                        , adjustedVal
                            * 7
                            |> clamp 0 3
                            |> round
                        )
            )
        |> Dict.fromList
    , newSeed
    )


init : Maybe String -> ( Model, Cmd Msg )
init mapJson =
    let
        initSeed : Seed
        initSeed =
            Random.initialSeed 41113

        parseMap : Maybe (Dict Point Tile)
        parseMap =
            mapJson
                |> Maybe.andThen
                    (Codec.decodeMap tileDecoder >> Result.toMaybe)

        ( map, seed ) =
            case parseMap of
                Just m ->
                    ( m, initSeed )

                Nothing ->
                    randomMap initSeed
    in
    ( Model
        map
        [ Entity.new ( 0, 0 ) ]
        (Render.newCamera |> Render.zoomCamera -0.7)
        ( 0, 0 )
        False
        False
        seed
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | HoverHex Point
    | ClickedHex Point
    | KeyPressed String
    | MouseDownChanged Bool
    | ClickedNewMap


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | entities =
                    model.entities
                        |> List.map (Entity.tickCooldown dt)
                        |> List.map Entity.move
              }
            , Cmd.none
            )

        HoverHex pos ->
            ( { model
                | hoverTile = pos
              }
            , Cmd.none
            )

        ClickedHex position ->
            ( { model
                | entities =
                    model.entities
                        |> List.map (Entity.pathfind (canMove model.tiles) position)
              }
            , Cmd.none
            )

        KeyPressed key ->
            case key of
                "a" ->
                    ( { model | camera = Render.moveCameraX -100 model.camera }
                    , Cmd.none
                    )

                "d" ->
                    ( { model | camera = Render.moveCameraX 100 model.camera }
                    , Cmd.none
                    )

                "w" ->
                    ( { model | camera = Render.moveCameraY -100 model.camera }
                    , Cmd.none
                    )

                "s" ->
                    ( { model | camera = Render.moveCameraY 100 model.camera }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        MouseDownChanged down ->
            ( { model | mouseDown = down }, Cmd.none )

        ClickedNewMap ->
            let
                ( newMap, newSeed ) =
                    randomMap model.seed
            in
            ( { model | tiles = newMap, seed = newSeed }, Ports.storeMap (Codec.encodeMap tileEncoder newMap) )



-- VIEW


viewTile : List (Svg.Attribute Msg) -> ( Point, Tile ) -> Svg Msg
viewTile attrs ( position, _ ) =
    Svg.g [ Render.hexTransform position ]
        [ Render.viewHex
            ([ Svg.Events.onMouseOver (HoverHex position)
             , Svg.Events.onClick (ClickedHex position)
             , Svg.Attributes.class "tile"
             ]
                ++ attrs
            )
        ]


viewTiles : Int -> Dict Point Tile -> Svg Msg
viewTiles level tiles =
    let
        pointString : Point -> String
        pointString point =
            String.fromInt (Tuple.first point) ++ ", " ++ String.fromInt (Tuple.second point)
    in
    Svg.Keyed.node "g"
        [ Svg.Attributes.class "tiles"
        , Svg.Attributes.class ("level-" ++ String.fromInt level)
        ]
        (tiles
            |> Dict.toList
            |> List.filter (Tuple.second >> (\tile -> tile >= level))
            |> List.map (\( pos, tile ) -> ( pointString pos, viewTile [] ( pos, tile ) ))
        )


viewEntity : Entity -> Svg msg
viewEntity entity =
    Svg.g [ Render.hexTransform entity.position ]
        [ Svg.circle
            [ Svg.Attributes.cx "0"
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r "70"
            , Svg.Attributes.fill "red"
            ]
            []
        , Render.viewValidPath (List.map (\p -> Point.subtract p entity.position) (entity.position :: entity.path))
        ]


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
    main_
        [ Html.Attributes.id "app"
        ]
        [ Html.button [ Html.Events.onClick ClickedNewMap ] [ Html.text "New map" ]
        , Render.svg
            [ Svg.Attributes.class "game-svg"
            , Svg.Events.onMouseDown (MouseDownChanged True)
            , Svg.Events.onMouseUp (MouseDownChanged False)
            ]
            [ Svg.defs [] [ gooFilter ]
            , Render.camera model.camera
                [ Svg.Attributes.class "camera" ]
                [ Svg.Lazy.lazy2 viewTiles 0 model.tiles
                , Svg.Lazy.lazy2 viewTiles 1 model.tiles
                , Svg.Lazy.lazy2 viewTiles 2 model.tiles
                , Svg.Lazy.lazy2 viewTiles 3 model.tiles
                , Svg.g [] (List.map viewEntity model.entities)
                ]
            ]
        ]



-- DECODERS


tileDecoder : Decoder Tile
tileDecoder =
    Json.Decode.int


keyPressDecoder : Decoder Msg
keyPressDecoder =
    Json.Decode.map KeyPressed
        (Json.Decode.field "key" Json.Decode.string)



-- ENCODERS


tileEncoder : Tile -> Json.Encode.Value
tileEncoder tile =
    Json.Encode.int tile



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyPress keyPressDecoder
        ]



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
