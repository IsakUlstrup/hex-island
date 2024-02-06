module Engine.Point exposing (Point, add, distance, isValid, neighbours)

{-| Axial coordinate
-}


type alias Point =
    ( Int, Int )


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
