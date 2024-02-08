module Engine.Path exposing (Node, Path, f, pathfind)

import Dict exposing (Dict)
import Engine.Point as Point exposing (Point)



-- TODO: Add parent point reference


type alias Node =
    { g : Int
    , h : Int
    , totalCost : Int
    , parent : Point
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
    , path : Maybe (List Point)
    }


{-| Initialize path with start point in open list
-}
init : Point -> Point -> Path
init from to =
    Path (Dict.singleton from (Node (Point.distance from from) (Point.distance from to) 0 from)) Dict.empty from to Nothing


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
    let
        -- Find cheapest node
        cheapest : Maybe Int
        cheapest =
            nodes
                |> Dict.toList
                |> List.map (Tuple.second >> f)
                |> List.sort
                |> List.head
    in
    cheapest
        |> Maybe.andThen
            (\c ->
                -- of all cheapest nodes, sort by h value and return first
                nodes
                    |> Dict.toList
                    |> List.filter (\( _, n ) -> f n == c)
                    |> List.sortBy (Tuple.second >> .h)
                    |> List.head
            )


{-| Find neighbouring points that can be moved to and are not in closed list, add them to open list
-}
addNeighboursToOpen : (Point -> Bool) -> ( Point, Node ) -> Path -> Path
addNeighboursToOpen canMove ( position, node ) path =
    { path
        | open =
            Point.neighbours position
                |> List.filter (\p -> (Dict.member p path.closed |> not) && canMove p)
                |> List.map
                    (\p ->
                        ( p, Node (Point.distance path.from p) (Point.distance path.to p) (node.totalCost + 1) position )
                    )
                |> Dict.fromList
                |> Dict.union path.open
    }


reconstructPath : ( Point, Node ) -> Dict Point Node -> List Point -> List Point
reconstructPath ( position, node ) nodes acum =
    if node.parent == position then
        position :: acum

    else
        case Dict.get node.parent nodes of
            Just parent ->
                position
                    :: acum
                    |> reconstructPath ( node.parent, parent ) nodes

            Nothing ->
                position :: acum


{-| Main pathfinding loop
-}
step : (Point -> Bool) -> Path -> Path
step canMove path =
    case ( findCheapest path.open, Dict.member path.to path.closed ) of
        ( Just cheapest, False ) ->
            if Tuple.first cheapest == path.to then
                { path | path = Just (reconstructPath cheapest path.closed []) }

            else
                path
                    |> addNeighboursToOpen canMove cheapest
                    |> moveToClosed cheapest
                    |> step canMove

        _ ->
            path
