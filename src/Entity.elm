module Entity exposing (Entity, EntityData(..), move, new, pathfind, tickCooldown)

import Engine.Path as Path
import Engine.Point exposing (Point)


type alias Entity =
    { position : Point
    , data : EntityData
    }


type EntityData
    = Human HumanData
    | Item
    | Building


new : Point -> Entity
new position =
    Entity position (Human (HumanData [] 0))


updateData : (EntityData -> EntityData) -> Entity -> Entity
updateData f entity =
    { entity | data = f entity.data }



-- HUMAN


type alias HumanData =
    { path : List Point
    , cooldown : Int
    }


tickCooldown : Float -> Entity -> Entity
tickCooldown dt entity =
    case entity.data of
        Human humanData ->
            updateData (always (Human { humanData | cooldown = humanData.cooldown - round dt |> max 0 })) entity

        _ ->
            entity


move : Entity -> Entity
move entity =
    case entity.data of
        Human humanData ->
            case ( humanData.path, humanData.cooldown ) of
                ( p :: ps, 0 ) ->
                    { entity
                        | position = p
                    }
                        |> updateData
                            (always
                                (Human
                                    { humanData
                                        | path = ps
                                        , cooldown = 200
                                    }
                                )
                            )

                _ ->
                    entity

        _ ->
            entity


pathfind : (Point -> Point -> Bool) -> Point -> Entity -> Entity
pathfind canMove target entity =
    case entity.data of
        Human humanData ->
            let
                path : Maybe (List Point)
                path =
                    Path.pathfind canMove entity.position target
                        |> .path
            in
            case path of
                Just validPath ->
                    entity |> updateData (always (Human { humanData | path = validPath }))

                Nothing ->
                    entity

        _ ->
            entity
