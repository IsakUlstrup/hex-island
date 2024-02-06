module Engine.Path exposing (pathfind)

import Engine.Point as Point exposing (Point)


fCost : Point -> Point -> Point -> Int
fCost from to position =
    let
        g : Int
        g =
            Point.distance from position

        h : Int
        h =
            Point.distance to position
    in
    g + h


pathfind : (Point -> Bool) -> Point -> Point -> List ( Point, Int )
pathfind canMove from to =
    if from == to then
        []

    else
        Point.neighbours from
            |> List.filter canMove
            |> List.map (\p -> ( p, fCost from to p ))
