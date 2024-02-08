module Engine.Path exposing (Node, Path, f, pathfind, toList)

import Dict exposing (Dict)
import Engine.Point as Point exposing (Point)



-- TODO: Add parent point reference


type alias Node =
    { g : Int
    , h : Int
    , totalCost : Int
    }


{-| Derive f cost of node
-}
f : Node -> Int
f node =
    node.g + node.h


type alias Path =
    { open : Dict Point Node
    , closed : Dict Point Node
    , from : Point
    , to : Point
    }


toList : Path -> List ( Point, Node )
toList path =
    Dict.toList path.open ++ Dict.toList path.closed


{-| Initialize path with start point in open list
-}
init : Point -> Point -> Path
init from to =
    Path (Dict.singleton from (Node (Point.distance from from) (Point.distance from to) 0)) Dict.empty from to


{-| Find shortest path between two points using A\*
-}
pathfind : (Point -> Bool) -> Point -> Point -> Path
pathfind canMove from to =
    if canMove to then
        init from to |> step canMove

    else
        init from to


{-| Move node from open to closed
-}
moveToClosed : ( Point, Node ) -> Path -> Path
moveToClosed ( position, node ) path =
    { path
        | open = path.open |> Dict.remove position
        , closed = path.closed |> Dict.insert position node
    }


{-| Find the cheapest node to move to
-}
findCheapest : Dict Point Node -> Maybe ( Point, Node )
findCheapest nodes =
    -- TODO: Sort by h if multiple nodes share lowest f
    nodes
        |> Dict.toList
        |> List.sortBy (Tuple.second >> f)
        |> List.head


{-| Find neighbouring points that can be moved to and are not in closed list, add them to open list
-}
addNeighboursToOpen : (Point -> Bool) -> ( Point, Node ) -> Path -> Path
addNeighboursToOpen canMove ( position, node ) path =
    { path
        | open =
            Point.neighbours position
                |> List.filter (\p -> (Dict.member p path.closed |> not) && canMove p)
                -- |> List.filter (\p -> Dict.member p path.open |> not)
                |> List.map
                    (\p ->
                        ( p, Node (Point.distance path.from p) (Point.distance path.to p) (node.totalCost + 1) )
                    )
                |> Dict.fromList
                |> Dict.union path.open
    }


{-| Main pathfinding loop
-}
step : (Point -> Bool) -> Path -> Path
step canMove path =
    case ( findCheapest path.open, Dict.member path.to path.closed ) of
        ( Just cheapest, False ) ->
            if Tuple.first cheapest == path.to then
                path

            else
                path
                    |> addNeighboursToOpen canMove cheapest
                    |> moveToClosed cheapest
                    |> step canMove

        _ ->
            path
