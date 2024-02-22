module Engine.Noise exposing (Vector2, generateCircle, noiseList)

import Engine.Point as Point exposing (Point)
import Engine.Render as Render
import Random exposing (Generator, Seed)


type alias Vector2 =
    { x : Float
    , y : Float
    }


{-| Cantor pairing function, convert x and y coordinates to a single integer
-}
cantorFunction : Int -> Int -> Int
cantorFunction x y =
    (x + y) * (x + y + 1) // 2 + y


{-| Generate a pseudo-random unit vector based on x and y coordinates
-}
randomUnitVector : Int -> Int -> Generator Vector2
randomUnitVector x y =
    Random.andThen
        (\r ->
            let
                theta =
                    cantorFunction x y
                        + r
                        |> toFloat
            in
            Random.constant (Vector2 (cos theta) (sin theta))
        )
        (Random.int -1000 1000)


getDotProduct : Float -> Float -> Int -> Int -> Generator Float
getDotProduct x y vert_x vert_y =
    let
        d_vect =
            { x = x - toFloat vert_x
            , y = y - toFloat vert_y
            }
    in
    Random.andThen
        (\g_vect ->
            Random.constant (d_vect.x * g_vect.x + d_vect.y * g_vect.y)
        )
        (randomUnitVector vert_x vert_y)


smootherstep : Float -> Float
smootherstep t =
    t * t * t * (t * (t * 6.0 - 15.0) + 10.0)


interp : Float -> Float -> Float -> Float
interp x a b =
    a + smootherstep x * (b - a)


noise : Seed -> Point -> Float
noise seed position =
    let
        scale : Float
        scale =
            0.0005

        ( x, y ) =
            Render.pointToPixel position
                |> Tuple.mapBoth (\n -> n * scale) (\n -> n * scale)

        floorX : Int
        floorX =
            floor x

        floorY : Int
        floorY =
            floor y

        topLeft : Float
        topLeft =
            Random.step (getDotProduct x y floorX floorY) seed
                |> Tuple.first

        topRight : Float
        topRight =
            Random.step (getDotProduct x y (floorX + 1) floorY) seed
                |> Tuple.first

        bottomLeft : Float
        bottomLeft =
            Random.step (getDotProduct x y floorX (floorY + 1)) seed
                |> Tuple.first

        bottomRight : Float
        bottomRight =
            Random.step (getDotProduct x y (floorX + 1) (floorY + 1)) seed
                |> Tuple.first

        xt =
            interp (x - toFloat floorX) topLeft topRight

        xb =
            interp (x - toFloat floorX) bottomLeft bottomRight
    in
    interp (y - toFloat floorY) xt xb + 1 / 2


noiseList : List Point -> Generator (List ( Point, Float ))
noiseList positions =
    Random.andThen
        (\seed -> Random.constant (List.map (\pos -> ( pos, noise seed pos )) positions))
        Random.independentSeed


generateCircle : Int -> Generator (List ( Point, Float ))
generateCircle radius =
    Random.andThen
        (\seed -> Random.constant (List.map (\pos -> ( pos, noise seed pos )) (Point.circle radius ( 0, 0 ))))
        Random.independentSeed
