module Engine.Point exposing (Point, add, circle, distance, fromFloat, isValid, neighbours, subtract)

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


{-| Get direction given hex side
-}
direction : Int -> Point
direction dir =
    case dir of
        0 ->
            ( 1, -1 )

        1 ->
            ( 1, 0 )

        2 ->
            ( 0, 1 )

        3 ->
            ( -1, 1 )

        4 ->
            ( -1, 0 )

        _ ->
            ( 0, -1 )


{-| Scale point
-}
scale : Int -> Point -> Point
scale i ( x1, y1 ) =
    ( x1 * i, y1 * i )


{-| Returns a ring around given point with given radius
-}
ring : Point -> Int -> List Point
ring center radius =
    let
        getDirection s =
            case s of
                0 ->
                    4

                1 ->
                    5

                2 ->
                    0

                3 ->
                    1

                4 ->
                    2

                _ ->
                    3

        start s =
            add center (scale radius (direction (getDirection s)))

        side s =
            List.map (\i -> add (start s) (scale i (direction s))) (List.range 0 radius)
    in
    List.concatMap side (List.range 0 6)


circle : Int -> Point -> List Point
circle radius center =
    List.range 0 radius
        |> List.concatMap (ring center)
