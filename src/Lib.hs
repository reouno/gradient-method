module Lib
    (
    ) where


h :: Double
h = 0.00001

epsilon :: Double
epsilon = 0.00001

differential :: (Double -> Double) -> Double -> Double
differential f a = (f (a + h) - f a) / h

find_min :: (Double -> Double) -> Double -> Double -> Double
find_min f a g =
    let
        a' = a - epsilon * g
        g' = differential f a'
    in
        if g * g' < 0
            then a
            else find_min f a' g