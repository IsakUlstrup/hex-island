module Engine.Path exposing (Node, Path, f, pathfind, toList)

import Dict exposing (Dict)
import Engine.Point as Point exposing (Point)


type alias Node =
    { g : Int
    , h : Int
    }


{-| Derive f cost of node
-}
f : Node -> Int
f node =
    node.g + node.h


type alias Path =
    { open : Dict Point Node
    , closed : Dict Point Node
    }


toList : Path -> List ( Point, Node )
toList path =
    Dict.toList path.open ++ Dict.toList path.closed


{-| Initialize path with start point in open list
-}
init : Point -> Path
init from =
    Path (Dict.singleton from (Node 0 0)) Dict.empty


{-| Find shortest path between two points using A\*
-}
pathfind : (Point -> Bool) -> Point -> Point -> Path
pathfind canMove from to =
    init from |> step canMove from to


{-| Move node from open to closed
-}
moveToClosed : ( Point, Node ) -> Path -> Path
moveToClosed ( position, node ) path =
    { path
        | open = path.open |> Dict.remove position
        , closed = path.closed |> Dict.insert position node
    }


{-| Main pathfinding loop
-}
step : (Point -> Bool) -> Point -> Point -> Path -> Path
step canMove from to path =
    let
        sortedOpen : List ( Point, Node )
        sortedOpen =
            path.open
                |> Dict.toList
                |> List.sortBy (Tuple.second >> f)
    in
    case sortedOpen of
        [] ->
            path

        cheapest :: _ ->
            let
                -- get neighbouring points that are not in closed list and is moveable
                neighbours : List ( Point, Node )
                neighbours =
                    Point.neighbours (Tuple.first cheapest)
                        |> List.filter (\p -> (Dict.member p path.closed |> not) && canMove p)
                        |> List.map (\p -> ( p, Node (Point.distance from p) (Point.distance to p) ))
            in
            { path
                | open =
                    Dict.union path.open (Dict.fromList neighbours)
            }
                |> moveToClosed cheapest
                |> step canMove from to
