module Engine.Point exposing (Point, add, isValid, neighbours)

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
