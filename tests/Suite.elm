module Suite exposing (point)

import Engine.Point as Point
import Expect
import Fuzz exposing (int)
import Test exposing (Test, describe, fuzz2, test)


point : Test
point =
    describe "The Point module"
        [ describe "Point.isValid"
            [ test "hardcoded valid point" <|
                \_ ->
                    Point.isValid ( 1, -1 )
                        |> Expect.equal True
            , fuzz2 int int "Check if random point is valid" <|
                \q r ->
                    let
                        shouldBeValid : Bool
                        shouldBeValid =
                            q + r + (-q - r) == 0
                    in
                    Point.isValid ( q, r )
                        |> Expect.equal shouldBeValid
            ]
        , describe "Point.add"
            [ test "Add two hardcoded points together, should be (0, 0)" <|
                \_ ->
                    Point.add ( 1, -1 ) ( -1, 1 )
                        |> Expect.equal ( 0, 0 )
            ]
        , describe "Point.distance"
            [ test "Distance between (0, 0) and (2, -2)" <|
                \_ ->
                    Point.distance ( 0, 0 ) ( 2, -2 )
                        |> Expect.equal 2
            , test "Distance between (0, 0) and (0, 0)" <|
                \_ ->
                    Point.distance ( 0, 0 ) ( 0, 0 )
                        |> Expect.equal 0
            ]
        , describe "Point.neighbours"
            [ test "Get neighbours of (0, 0)" <|
                \_ ->
                    Point.neighbours ( 0, 0 )
                        |> Expect.equalLists
                            [ ( 0, -1 )
                            , ( 1, -1 )
                            , ( 1, 0 )
                            , ( 0, 1 )
                            , ( -1, 1 )
                            , ( -1, 0 )
                            ]
            , test "Get neighbours of (1, -1)" <|
                \_ ->
                    Point.neighbours ( 1, -1 )
                        |> Expect.equalLists
                            [ ( 1, -2 )
                            , ( 2, -2 )
                            , ( 2, -1 )
                            , ( 1, 0 )
                            , ( 0, 0 )
                            , ( 0, -1 )
                            ]
            ]
        ]
