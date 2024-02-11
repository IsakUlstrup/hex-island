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


type alias Tile =
    Int


tileToString : Tile -> String
tileToString tile =
    String.fromInt tile


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
                    Dict.empty
    in
    ( Model
        initMap
        ( 0, 0 )
        ( 0, 0 )
        False
        1
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


viewTile : List (Svg.Attribute Msg) -> ( Point, Tile ) -> Svg Msg
viewTile attrs ( position, tile ) =
    Svg.g [ Render.hexTransform position ]
        [ Render.viewHex
            ([ Svg.Events.onMouseOver (HoverHex position)
             , Svg.Events.onClick (ClickedHex position)
             , Svg.Attributes.class "tile"
             , Svg.Attributes.class ("level" ++ tileToString tile)
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
    if level == 0 then
        Svg.g [ Svg.Attributes.class "tiles", Svg.Attributes.style "opacity: 0.5" ]
            (tiles
                |> Dict.toList
                |> List.filter (\( _, tile ) -> tile >= level)
                |> List.map (viewTile [])
            )

    else
        Svg.g [ Svg.Attributes.class "tiles" ]
            (tiles
                |> Dict.toList
                |> List.filter (\( _, tile ) -> tile >= level)
                |> List.map (viewTile [])
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
            [ Svg.Lazy.lazy2 viewTiles 0 model.tiles
            , Svg.Lazy.lazy2 viewTiles 1 model.tiles
            , Svg.Lazy.lazy2 viewTiles 2 model.tiles
            , Svg.Lazy.lazy2 viewTiles 3 model.tiles
            , viewPath model.tiles ( 0, 0 ) model.hoverTile
            ]
        ]


viewEditor : Model -> Html Msg
viewEditor model =
    Html.div [ Html.Attributes.class "editor" ]
        [ Html.section [ Html.Attributes.class "sidebar" ]
            [ Html.h1 [] [ Html.text "Tile" ]
            , Html.ul []
                [ Html.li [ Html.Events.onClick (ClickedSidebarTile 0) ] [ Html.text "0" ]
                , Html.li [ Html.Events.onClick (ClickedSidebarTile 1) ] [ Html.text "1" ]
                , Html.li [ Html.Events.onClick (ClickedSidebarTile 2) ] [ Html.text "2" ]
                , Html.li [ Html.Events.onClick (ClickedSidebarTile 3) ] [ Html.text "3" ]
                , Html.li [ Html.Events.onClick (ClickedSidebarTile -1) ] [ Html.text "-1" ]
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
                , Svg.Lazy.lazy (viewTiles 0) model.tiles
                , Svg.Lazy.lazy (viewTiles 1) model.tiles
                , Svg.Lazy.lazy (viewTiles 2) model.tiles
                , Svg.Lazy.lazy (viewTiles 3) model.tiles
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
