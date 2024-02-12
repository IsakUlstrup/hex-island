module Engine.Render exposing
    ( camera
    , hexTransform
    , square
    , svg
    , viewDebugPath
    , viewHex
    , viewValidPath
    )

import Dict
import Engine.Path exposing (Node, Path)
import Engine.Point as Point exposing (Point)
import Html exposing (Html)
import Svg exposing (Attribute, Svg)
import Svg.Attributes


{-| Hex size constant

If you want to resize hexes, use transforms and or camera zoom

-}
hexSize : Float
hexSize =
    100


{-| Get the center of a given point in screen coordinates
-}
pointToPixel : Point -> ( Int, Int )
pointToPixel ( q, r ) =
    ( hexSize * (3 / 2 * toFloat q) |> round
    , hexSize * (sqrt 3 / 2 * toFloat q + sqrt 3 * toFloat r) |> round
    )


{-| Convert a list of floats to a Svg points attribute
-}
cornerListToPoints : List ( Float, Float ) -> Attribute msg
cornerListToPoints points =
    let
        tupleToString : ( Float, Float ) -> String
        tupleToString ( x, y ) =
            String.fromInt (round x) ++ "," ++ String.fromInt (round y)
    in
    points
        |> List.map tupleToString
        |> String.join " "
        |> Svg.Attributes.points


{-| Calculate hex corners in screen coordinates
-}
generateHexCorners : List ( Float, Float )
generateHexCorners =
    let
        corner : Float -> ( Float, Float )
        corner cornerNumber =
            ( hexSize * cos (degrees <| 60 * cornerNumber)
            , hexSize * sin (degrees <| 60 * cornerNumber)
            )
    in
    List.range 0 5
        |> List.map (toFloat >> corner)


{-| Render a hexagon

If you want to translate it, pass hexTransform position as an attribute

-}
viewHex : List (Attribute msg) -> Svg msg
viewHex attrs =
    Svg.polygon (cornerListToPoints generateHexCorners :: attrs) []


{-| Calculate & set svg transform in screen coordinates
-}
hexTransform : Point -> Attribute msg
hexTransform position =
    let
        ( x, y ) =
            pointToPixel position
    in
    Svg.Attributes.style
        ("transform: translate("
            ++ String.fromInt x
            ++ "px, "
            ++ String.fromInt y
            ++ "px)"
        )


{-| Camera transform
-}
cameraTransform : ( Int, Int ) -> Attribute msg
cameraTransform ( x, y ) =
    Svg.Attributes.style
        ("transform: translate("
            ++ String.fromInt -x
            ++ "px, "
            ++ String.fromInt -y
            ++ "px)"
        )


{-| Camera element
-}
camera : ( Int, Int ) -> List (Attribute msg) -> List (Svg msg) -> Svg msg
camera position attrs children =
    Svg.g (cameraTransform position :: attrs) children


svg : List (Attribute msg) -> List (Svg msg) -> Html msg
svg attrs children =
    Svg.svg
        ([ Svg.Attributes.viewBox "-1000 -1000 2000 2000"
         , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
         ]
            ++ attrs
        )
        children


square : ( Int, Int ) -> List Point
square ( x, y ) =
    let
        center : Point
        center =
            ( (2 / 3 * toFloat x) / hexSize |> round
            , (-1 / 3 * toFloat x + sqrt 3 / 3 * toFloat y) / hexSize |> round
            )

        radius : Int
        radius =
            5
    in
    (List.range -radius radius
        |> List.concatMap
            (\r ->
                List.range -radius radius
                    |> List.map (\q -> ( q, r ))
            )
    )
        |> List.filter Point.isValid
        |> List.map (Point.add center)



-- PATHFINDING DEBUG


viewPathNode : List (Svg.Attribute msg) -> ( Point, Node ) -> Svg msg
viewPathNode attrs ( pos, node ) =
    let
        ( toX, toY ) =
            pointToPixel (Point.subtract node.parent pos)
    in
    Svg.g
        ([ hexTransform pos
         , Svg.Attributes.fontSize "2rem"
         , Svg.Attributes.textAnchor "middle"
         , Svg.Attributes.class "path-node"
         ]
            ++ attrs
        )
        [ viewHex
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


viewValidPath : List Point -> Svg msg
viewValidPath positions =
    let
        points : String
        points =
            positions
                |> List.map pointToPixel
                |> List.map (\( q, r ) -> String.fromInt q ++ "," ++ String.fromInt r)
                |> String.join " "
    in
    Svg.polyline
        [ Svg.Attributes.points points
        , Svg.Attributes.class "path"
        ]
        []


viewDebugPath : Path -> Svg msg
viewDebugPath path =
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
                    [ viewValidPath validPath ]

                Nothing ->
                    []
            )
        ]
