module MyFirstTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import NumbersEval exposing (isEven)


suite : Test
suite =
    describe "Numbers are either odd or even"
    [ test "Odd ones are not even" <|
        \() ->
            Expect.false "Expected 3 to be odd" (isEven 3)
    , test "Even ones are even" <|
        \() ->
            Expect.true "Expected 4 to be even" (isEven 4)
    ]

