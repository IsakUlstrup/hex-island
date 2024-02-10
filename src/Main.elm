module Main exposing (Model, Msg, Tile, main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Engine.Codec as Codec
import Engine.Path as Path exposing (Node, Path)
import Engine.Point as Point exposing (Point)
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
import Svg.Lazy



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
    , editor : Bool
    , editorSelectedTile : Tile
    , mouseDown : Bool
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
                    Dict.fromList
                        [ ( ( 0, 0 ), Grass )
                        , ( ( 1, -1 ), Grass )
                        , ( ( 0, 1 ), Grass )
                        , ( ( 1, 0 ), Water )
                        , ( ( 2, -2 ), Water )
                        , ( ( 2, -1 ), Water )
                        , ( ( -1, 0 ), Grass )
                        ]
    in
    ( Model
        initMap
        ( 0, 0 )
        ( 0, 0 )
        False
        Grass
        False
    , Cmd.none
    )



-- UPDATE


type Msg
    = HoverHex Point
    | ClickedHex Point
    | ClickedSidebarTile Tile
    | KeyPressed String
    | MouseDownChanged Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HoverHex pos ->
            if model.editor && model.mouseDown then
                let
                    newMap : Dict Point Tile
                    newMap =
                        model.tiles |> Dict.insert pos model.editorSelectedTile
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

        ClickedHex position ->
            if model.editor then
                let
                    newMap : Dict Point Tile
                    newMap =
                        model.tiles |> Dict.insert position model.editorSelectedTile
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



-- VIEW


viewTile : ( Point, Tile ) -> Svg Msg
viewTile ( position, tile ) =
    Svg.g [ Render.hexTransform position ]
        [ Render.viewHex
            [ Svg.Events.onMouseOver (HoverHex position)
            , Svg.Events.onClick (ClickedHex position)
            , Svg.Attributes.class "tile"
            , Svg.Attributes.class (tileToString tile)
            , Svg.Attributes.transform "scale(1.02)"
            ]
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


viewTiles : Dict Point Tile -> Svg Msg
viewTiles tiles =
    Svg.g [ Svg.Attributes.class "tiles" ]
        (tiles
            |> Dict.toList
            |> List.map viewTile
        )


viewPathNode : List (Svg.Attribute msg) -> ( Point, Node ) -> Svg msg
viewPathNode attrs ( pos, node ) =
    let
        ( toX, toY ) =
            Render.pointToPixel (Point.subtract node.parent pos)
    in
    Svg.g
        ([ Render.hexTransform pos
         , Svg.Attributes.fontSize "2rem"
         , Svg.Attributes.textAnchor "middle"
         , Svg.Attributes.class "path-node"
         ]
            ++ attrs
        )
        [ Render.viewHex
            [ Svg.Attributes.fillOpacity "0.2"
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
        , Svg.line
            [ Svg.Attributes.x1 "0"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 (String.fromInt toX)
            , Svg.Attributes.y2 (String.fromInt toY)
            , Svg.Attributes.strokeWidth "3"
            , Svg.Attributes.stroke "beige"
            ]
            []
        , Svg.circle
            [ Svg.Attributes.cx (String.fromInt toX)
            , Svg.Attributes.cy (String.fromInt toY)
            , Svg.Attributes.r "10"
            , Svg.Attributes.fill "beige"
            ]
            []

        -- , Svg.text_
        --     [ Svg.Attributes.y "15"
        --     , Svg.Attributes.fill "hsl(200, 75%, 50%)"
        --     ]
        --     [ Svg.text ("f: " ++ String.fromInt (Path.f node)) ]
        ]


viewPath2 : List Point -> Svg msg
viewPath2 positions =
    let
        points : String
        points =
            positions
                |> List.map Render.pointToPixel
                |> List.map (\( q, r ) -> String.fromInt q ++ "," ++ String.fromInt r)
                |> String.join " "
    in
    Svg.polyline
        [ Svg.Attributes.points points
        , Svg.Attributes.stroke "beige"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.strokeWidth "20"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.strokeLinecap "round"
        ]
        []


viewPath : Dict Point Tile -> Point -> Point -> Svg msg
viewPath tiles from to =
    let
        path : Path
        path =
            Path.pathfind (canMove tiles) from to

        debug : Bool
        debug =
            False
    in
    if debug then
        Svg.g []
            [ Svg.g []
                (path.closed
                    |> Dict.toList
                    |> List.map (viewPathNode [ Svg.Attributes.class "closed" ])
                )
            , Svg.g []
                (path.open
                    |> Dict.toList
                    |> List.map (viewPathNode [ Svg.Attributes.class "open" ])
                )
            , Svg.g []
                (case path.path of
                    Just validPath ->
                        [ viewPath2 validPath ]

                    Nothing ->
                        []
                )
            ]

    else
        Svg.g []
            (case path.path of
                Just validPath ->
                    [ viewPath2 validPath ]

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
        , Render.camera model.cameraPosition
            [ Svg.Attributes.class "camera" ]
            [ Svg.Lazy.lazy viewTiles
                (model.tiles
                    |> Dict.filter
                        (\_ tile ->
                            case tile of
                                Grass ->
                                    True

                                Water ->
                                    False
                        )
                )
            , Svg.Lazy.lazy viewTiles
                (model.tiles
                    |> Dict.filter
                        (\_ tile ->
                            case tile of
                                Water ->
                                    True

                                Grass ->
                                    False
                        )
                )
            , viewPath model.tiles ( 0, 0 ) model.hoverTile
            ]
        ]


viewEditor : Model -> Html Msg
viewEditor model =
    Html.div [ Html.Attributes.class "editor" ]
        [ Html.section [ Html.Attributes.class "sidebar" ]
            [ Html.h1 [] [ Html.text "Tile" ]
            , Html.ul []
                [ Html.li [ Html.Events.onClick (ClickedSidebarTile Grass) ] [ Html.text "Grass" ]
                , Html.li [ Html.Events.onClick (ClickedSidebarTile Water) ] [ Html.text "Water" ]
                ]
            ]
        , Render.svg
            [ Svg.Attributes.class "game-svg"
            , Svg.Events.onMouseDown (MouseDownChanged True)
            , Svg.Events.onMouseUp (MouseDownChanged False)
            ]
            [ Svg.defs [] [ gooFilter ]
            , Render.camera model.cameraPosition
                [ Svg.Attributes.class "camera" ]
                [ Svg.g [] (List.map viewGhostTile (Render.square model.cameraPosition))
                , Svg.Lazy.lazy viewTiles
                    (model.tiles
                        |> Dict.filter
                            (\_ tile ->
                                case tile of
                                    Grass ->
                                        True

                                    Water ->
                                        False
                            )
                    )
                , Svg.Lazy.lazy viewTiles
                    (model.tiles
                        |> Dict.filter
                            (\_ tile ->
                                case tile of
                                    Water ->
                                        True

                                    Grass ->
                                        False
                            )
                    )
                ]
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


grassDecoder : Decoder Tile
grassDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                case s of
                    "grass" ->
                        Json.Decode.succeed Grass

                    _ ->
                        Json.Decode.fail "invalid tile"
            )


waterDecoder : Decoder Tile
waterDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                case s of
                    "water" ->
                        Json.Decode.succeed Water

                    _ ->
                        Json.Decode.fail "invalid tile"
            )


tileDecoder : Decoder Tile
tileDecoder =
    Json.Decode.oneOf [ grassDecoder, waterDecoder ]


keyPressDecoder : Decoder Msg
keyPressDecoder =
    Json.Decode.map KeyPressed
        (Json.Decode.field "key" Json.Decode.string)



-- ENCODERS


tileEncoder : Tile -> Json.Encode.Value
tileEncoder tile =
    case tile of
        Grass ->
            Json.Encode.string "grass"

        Water ->
            Json.Encode.string "water"



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
