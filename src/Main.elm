module Main exposing (Model, Msg, Tile, main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Engine.Codec as Codec
import Engine.Path as Path exposing (Path)
import Engine.Point exposing (Point)
import Engine.Render as Render
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Decoder)
import Json.Encode
import Ports
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
    , cameraPosition : ( Int, Int )
    , cameraZoom : Float
    , hoverTile : Point
    , editor : Bool
    , editorSelectedTile : Maybe Tile
    , mouseDown : Bool
    , viewPathDebug : Bool
    }


init : Maybe String -> ( Model, Cmd Msg )
init mapJson =
    let
        initMap : Dict Point Tile
        initMap =
            case mapJson of
                Just map ->
                    case Codec.decodeMap tileDecoder map of
                        Ok validMap ->
                            validMap

                        Err _ ->
                            Dict.empty

                Nothing ->
                    Dict.empty
    in
    ( Model
        initMap
        ( 0, 0 )
        1
        ( 0, 0 )
        False
        Nothing
        False
        False
    , Cmd.none
    )



-- UPDATE


type Msg
    = HoverHex Point
    | ClickedHex Point
    | ClickedSidebarTile (Maybe Tile)
    | KeyPressed String
    | MouseDownChanged Bool
    | ToggledViewPathDebug Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HoverHex pos ->
            if model.editor && model.mouseDown then
                let
                    newMap : Dict Point Tile
                    newMap =
                        case model.editorSelectedTile of
                            Just t ->
                                model.tiles |> Dict.insert pos t

                            Nothing ->
                                model.tiles |> Dict.remove pos
                in
                ( { model | tiles = newMap }
                , Ports.storeMap (Codec.encodeMap tileEncoder newMap)
                )

            else
                ( { model
                    | hoverTile = pos
                  }
                , Cmd.none
                )

        ClickedHex pos ->
            if model.editor then
                let
                    newMap : Dict Point Tile
                    newMap =
                        case model.editorSelectedTile of
                            Just t ->
                                model.tiles |> Dict.insert pos t

                            Nothing ->
                                model.tiles |> Dict.remove pos
                in
                ( { model | tiles = newMap }
                , Ports.storeMap (Codec.encodeMap tileEncoder newMap)
                )

            else
                ( model, Cmd.none )

        ClickedSidebarTile tile ->
            ( { model | editorSelectedTile = tile }
            , Cmd.none
            )

        KeyPressed key ->
            case key of
                " " ->
                    ( { model | editor = not model.editor }, Cmd.none )

                "a" ->
                    ( { model | cameraPosition = Tuple.mapFirst (\x -> x - 100) model.cameraPosition }, Cmd.none )

                "d" ->
                    ( { model | cameraPosition = Tuple.mapFirst (\x -> x + 100) model.cameraPosition }, Cmd.none )

                "w" ->
                    ( { model | cameraPosition = Tuple.mapSecond (\y -> y - 100) model.cameraPosition }, Cmd.none )

                "s" ->
                    ( { model | cameraPosition = Tuple.mapSecond (\y -> y + 100) model.cameraPosition }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MouseDownChanged down ->
            ( { model | mouseDown = down }, Cmd.none )

        ToggledViewPathDebug flag ->
            ( { model | viewPathDebug = flag }, Cmd.none )



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


viewGhostTile : Point -> Svg Msg
viewGhostTile position =
    Render.viewHex
        [ Render.hexTransform position
        , Svg.Events.onMouseOver (HoverHex position)
        , Svg.Events.onClick (ClickedHex position)
        , Svg.Attributes.class "ghost-tile"
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.stroke "beige"
        , Svg.Attributes.strokeWidth "1"
        ]


viewTiles : Int -> Dict Point Tile -> Svg Msg
viewTiles level tiles =
    Svg.Keyed.node "g"
        [ Svg.Attributes.class "tiles"
        , Svg.Attributes.class ("level-" ++ String.fromInt level)
        ]
        (tiles
            |> Dict.toList
            |> List.filter (Tuple.second >> (\tile -> tile >= level))
            |> List.map (\( pos, tile ) -> ( String.fromInt (Tuple.first pos) ++ ", " ++ String.fromInt (Tuple.second pos), viewTile [] ( pos, tile ) ))
        )


viewPath : Bool -> Dict Point Tile -> Point -> Point -> Svg msg
viewPath debug tiles from to =
    let
        path : Path
        path =
            Path.pathfind (canMove tiles) from to
    in
    if debug then
        Render.viewDebugPath path

    else
        Svg.g []
            (case path.path of
                Just validPath ->
                    [ Render.viewValidPath validPath ]

                Nothing ->
                    []
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
        , Render.camera model.cameraZoom
            model.cameraPosition
            [ Svg.Attributes.class "camera" ]
            [ Svg.Lazy.lazy2 viewTiles 0 model.tiles
            , Svg.Lazy.lazy2 viewTiles 1 model.tiles
            , Svg.Lazy.lazy2 viewTiles 2 model.tiles
            , Svg.Lazy.lazy2 viewTiles 3 model.tiles
            , viewPath model.viewPathDebug model.tiles ( 0, 0 ) model.hoverTile
            ]
        ]


viewTilePresetButton : Maybe Tile -> Maybe Tile -> Html Msg
viewTilePresetButton selected tile =
    let
        label : String
        label =
            case tile of
                Just t ->
                    String.fromInt t

                Nothing ->
                    "x"
    in
    Html.button
        [ Html.Attributes.class ("preset-" ++ label)
        , Html.Attributes.class "tile-preset"
        , Html.Attributes.classList [ ( "selected", selected == tile ) ]
        , Html.Events.onClick (ClickedSidebarTile tile)
        ]
        [ Html.text label ]


viewEditor : Model -> Html Msg
viewEditor model =
    Html.div [ Html.Attributes.class "editor" ]
        [ Html.section [ Html.Attributes.class "sidebar" ]
            [ Html.h1 [] [ Html.text "Tile" ]
            , Html.div [ Html.Attributes.class "tile-presets" ]
                [ viewTilePresetButton model.editorSelectedTile Nothing
                , viewTilePresetButton model.editorSelectedTile (Just 0)
                , viewTilePresetButton model.editorSelectedTile (Just 1)
                , viewTilePresetButton model.editorSelectedTile (Just 2)
                , viewTilePresetButton model.editorSelectedTile (Just 3)
                ]
            , Html.div []
                [ Html.h1 [] [ Html.text "Path debug" ]
                , Html.input
                    [ Html.Attributes.type_ "checkbox"
                    , Html.Attributes.id "view-path-debug-checkbox"
                    , Html.Attributes.checked model.viewPathDebug
                    , Html.Events.onClick (ToggledViewPathDebug (not model.viewPathDebug))
                    ]
                    []
                , Html.label [ Html.Attributes.for "view-path-debug-checkbox" ] [ Html.text "Show path debug" ]
                ]
            ]
        , Render.svg
            [ Svg.Attributes.class "game-svg"
            , Svg.Events.onMouseDown (MouseDownChanged True)
            , Svg.Events.onMouseUp (MouseDownChanged False)
            ]
            [ Svg.defs [] [ gooFilter ]
            , Render.camera model.cameraZoom
                model.cameraPosition
                [ Svg.Attributes.class "camera" ]
                [ Svg.Lazy.lazy (viewTiles 0) model.tiles
                , Svg.Lazy.lazy (viewTiles 1) model.tiles
                , Svg.Lazy.lazy (viewTiles 2) model.tiles
                , Svg.Lazy.lazy (viewTiles 3) model.tiles
                , Svg.g [] (List.map viewGhostTile (Render.square model.cameraPosition))
                ]

            -- -- show a circle at the center of the screen, for debuging
            -- , Svg.circle
            --     [ Svg.Attributes.cx "0"
            --     , Svg.Attributes.cy "0"
            --     , Svg.Attributes.r "10"
            --     ]
            --     []
            ]
        ]


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ if model.editor then
            viewEditor model

          else
            viewGame model
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
    Browser.Events.onKeyPress keyPressDecoder



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
