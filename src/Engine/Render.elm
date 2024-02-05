module Engine.Render exposing
    ( hexTransform
    , viewHex
    )

import Engine.Point exposing (Point)
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
pointToPixel : Point -> ( Float, Float )
pointToPixel ( q, r ) =
    ( hexSize * (3 / 2 * toFloat q)
    , hexSize * (sqrt 3 / 2 * toFloat q + sqrt 3 * toFloat r)
    )


{-| Convert a list of floats to a Svg points attribute
-}
cornerListToPoints : List ( Float, Float ) -> Attribute msg
cornerListToPoints points =
    let
        tupleToString : ( Float, Float ) -> String
        tupleToString ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y
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
    Svg.Attributes.transform
        ("translate("
            ++ String.fromFloat x
            ++ ", "
            ++ String.fromFloat y
            ++ ")"
        )
