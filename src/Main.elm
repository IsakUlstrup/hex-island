module Main exposing (Model, Msg, Tile, main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Editor exposing (EditorState, Tool(..))
import Engine.Codec as Codec
import Engine.Noise as Noise
import Engine.Path as Path exposing (Path)
import Engine.Point as Point exposing (Point)
import Engine.Render as Render exposing (Camera)
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


applyTool : Tool -> Point -> Dict Point Tile -> Dict Point Tile
applyTool tool position tiles =
    case tool of
        Raise magnitude ->
            Dict.update position
                (\t ->
                    case t of
                        Just level ->
                            Just (level + magnitude |> min magnitude)

                        Nothing ->
                            Just (-1 + magnitude)
                )
                tiles

        Lower magnitude ->
            Dict.update position
                (Maybe.andThen
                    (\level ->
                        if (level - magnitude) < 0 then
                            Nothing

                        else
                            Just (level - magnitude |> max 0)
                    )
                )
                tiles

        Level height ->
            Dict.insert position height tiles

        Pan ->
            tiles



-- MODEL


type alias Model =
    { tiles : Dict Point Tile
    , camera : Camera
    , hoverTile : Point
    , editor : EditorState
    , mouseDown : Bool
    , viewPathDebug : Bool
    , seed : Seed
    }


randomMap : Seed -> ( Dict Point Tile, Seed )
randomMap seed =
    let
        radius : number
        radius =
            20

        ( map, newSeed ) =
            Random.step (Noise.generateCircle radius) seed

        distanceFalloff : Point -> Float
        distanceFalloff position =
            toFloat (Point.distance ( 0, 0 ) position) / radius
    in
    ( map
        |> List.filterMap
            (\( pos, val ) ->
                if val - distanceFalloff pos < 0 then
                    Nothing

                else
                    Just
                        ( pos
                        , (val - distanceFalloff pos)
                            * 6
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
        (Render.newCamera |> Render.zoomCamera -0.7)
        ( 0, 0 )
        Editor.init
        False
        False
        seed
    , Cmd.none
    )



-- UPDATE


type Msg
    = HoverHex Point
    | ClickedHex Point
    | ClickedSidebarTool Tool
    | KeyPressed String
    | MouseDownChanged Bool
    | ClickedNewMap


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HoverHex pos ->
            if model.editor.enabled && model.mouseDown then
                let
                    newMap : Dict Point Tile
                    newMap =
                        List.foldl (applyTool model.editor.selectedTool) model.tiles (pos :: Point.neighbours pos)
                in
                ( { model
                    | tiles = newMap
                    , hoverTile = pos
                  }
                , Ports.storeMap (Codec.encodeMap tileEncoder newMap)
                )

            else
                ( { model
                    | hoverTile = pos
                  }
                , Cmd.none
                )

        ClickedHex pos ->
            if model.editor.enabled then
                let
                    newMap : Dict Point Tile
                    newMap =
                        List.foldl (applyTool model.editor.selectedTool) model.tiles (pos :: Point.neighbours pos)
                in
                ( { model | tiles = newMap }
                , Ports.storeMap (Codec.encodeMap tileEncoder newMap)
                )

            else
                ( model, Cmd.none )

        ClickedSidebarTool tool ->
            ( { model | editor = Editor.setTool tool model.editor }
            , Cmd.none
            )

        KeyPressed key ->
            case key of
                " " ->
                    ( { model | editor = Editor.toggle model.editor }
                    , Cmd.none
                    )

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


svgClassList : List ( String, Bool ) -> Svg.Attribute msg
svgClassList classes =
    classes
        |> List.filter Tuple.second
        |> List.map Tuple.first
        |> String.join " "
        |> Svg.Attributes.class


viewGhostTile : List Point -> Point -> Svg Msg
viewGhostTile highlight position =
    Render.viewHex
        [ Render.hexTransform position
        , Svg.Events.onMouseOver (HoverHex position)
        , Svg.Events.onClick (ClickedHex position)
        , Svg.Attributes.class "ghost-tile"
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.stroke "beige"
        , Svg.Attributes.strokeWidth "1"
        , svgClassList [ ( "highlight", List.member position highlight ) ]
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


viewToolPresetButton : Tool -> Tool -> Html Msg
viewToolPresetButton selected tool =
    Html.button
        [ Html.Attributes.class ("preset-" ++ Editor.toolToString tool)
        , Html.Attributes.class "tool-preset"
        , Html.Attributes.classList [ ( "selected", selected == tool ) ]
        , Html.Events.onClick (ClickedSidebarTool tool)
        ]
        [ Html.text (Editor.toolToString tool) ]


viewEditor : Editor.EditorState -> Html Msg
viewEditor editor =
    Html.section
        [ Html.Attributes.class "sidebar"
        , Html.Attributes.classList [ ( "show-sidebar", editor.enabled ) ]
        ]
        [ Html.h1 [] [ Html.text "Tool" ]
        , Html.div [ Html.Attributes.class "tool-presets" ]
            [ viewToolPresetButton editor.selectedTool (Editor.Raise 1)
            , viewToolPresetButton editor.selectedTool (Editor.Lower 1)
            , viewToolPresetButton editor.selectedTool (Editor.Level 1)
            , viewToolPresetButton editor.selectedTool Editor.Pan
            ]
        , Html.button [ Html.Events.onClick ClickedNewMap ] [ Html.text "new map" ]

        -- , Html.div []
        --     [ Html.h1 [] [ Html.text "Path debug" ]
        --     , Html.input
        --         [ Html.Attributes.type_ "checkbox"
        --         , Html.Attributes.id "view-path-debug-checkbox"
        --         , Html.Attributes.checked debug
        --         , Html.Events.onClick (ToggledViewPathDebug (not debug))
        --         ]
        --         []
        --     , Html.label [ Html.Attributes.for "view-path-debug-checkbox" ] [ Html.text "Show path debug" ]
        --     ]
        ]


view : Model -> Html Msg
view model =
    main_
        [ Html.Attributes.id "app"
        ]
        [ viewEditor model.editor
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
                , Svg.g []
                    (if model.editor.enabled then
                        List.map (viewGhostTile (Point.circle model.editor.brushRadius model.hoverTile)) (Render.square model.camera)

                     else
                        [ viewPath model.viewPathDebug model.tiles ( 0, 0 ) model.hoverTile ]
                    )
                ]

            -- Center screen indicator
            -- , Svg.circle [ Svg.Attributes.cx "0", Svg.Attributes.cy "0", Svg.Attributes.r "10" ] []
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
