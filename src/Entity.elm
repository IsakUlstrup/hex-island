module Entity exposing (Entity, move, new, pathfind, tickCooldown)

import Engine.Path as Path
import Engine.Point exposing (Point)


type alias Entity =
    { position : Point
    , path : List Point
    , cooldown : Int
    }


new : Point -> Entity
new position =
    Entity position [] 0


tickCooldown : Float -> Entity -> Entity
tickCooldown dt entity =
    { entity | cooldown = entity.cooldown - round dt |> max 0 }


move : (Point -> Point -> Bool) -> Entity -> Entity
move canMove entity =
    case ( entity.path, entity.cooldown ) of
        ( p :: ps, 0 ) ->
            if canMove entity.position p then
                { entity
                    | position = p
                    , path = ps
                    , cooldown = 200
                }

            else
                entity

        _ ->
            entity


pathfind : (Point -> Point -> Bool) -> Point -> Entity -> Entity
pathfind canMove target entity =
    let
        path : Maybe (List Point)
        path =
            Path.pathfind canMove entity.position target
                |> .path
    in
    case path of
        Just validPath ->
            { entity | path = validPath }

        Nothing ->
            entity
