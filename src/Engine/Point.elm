module Engine.Point exposing (Point, add, distance, fromFloat, isValid, neighbours, subtract)

{-| Axial coordinate
-}


type alias Point =
    ( Int, Int )


{-| Create a new point from float values, round to nearest valid point
-}
fromFloat : ( Float, Float ) -> Point
fromFloat ( x, y ) =
    let
        z : Float
        z =
            -x - y

        -- rounded point
        ( rx, ry, rz ) =
            ( round x, round y, round z )

        -- diierence between input point and rounded point
        ( dx, dy, dz ) =
            ( abs (toFloat rx - x), abs (toFloat ry - y), abs (toFloat rz - z) )
    in
    if dx > dy && dx > dz then
        ( -ry - rz, ry )

    else if dy > dz then
        ( rx, -rx - rz )

    else
        ( rx, ry )


{-| Check if point is valid
-}
isValid : Point -> Bool
isValid ( q, r ) =
    q + r + (-q - r) == 0


{-| Add two points togheter
-}
add : Point -> Point -> Point
add p1 p2 =
    ( Tuple.first p1 + Tuple.first p2, Tuple.second p1 + Tuple.second p2 )


subtract : Point -> Point -> Point
subtract p1 p2 =
    ( Tuple.first p1 - Tuple.first p2, Tuple.second p1 - Tuple.second p2 )


distance : Point -> Point -> Int
distance from to =
    let
        ( q, r ) =
            subtract from to
    in
    (abs q + abs (q + r) + abs r) // 2


{-| Get all six neighbouring points
-}
neighbours : Point -> List Point
neighbours point =
    [ ( 0, -1 )
    , ( 1, -1 )
    , ( 1, 0 )
    , ( 0, 1 )
    , ( -1, 1 )
    , ( -1, 0 )
    ]
        |> List.map (add point)
