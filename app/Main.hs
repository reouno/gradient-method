module Main where

import Lib

main :: IO ()
main = do
    putStrLn "----------------Gradient method（勾配法）----------------\n"
    putStrLn "This program is sample of \"Gradient method\"."
    putStrLn "This sample is for function of one variable."
    let f x = x^2
    putStrLn "Function:  f(x) = x^2"
    let a = (1)
    putStrLn $ "Initial value:  a = " ++ (show a)
    let minValue = find_min f a (differential f a)
    putStrLn $ "Local minimum value:  min = " ++ (show minValue)
    putStrLn "\n\n----------------微分の例（勾配を求める例）----------------\n"
    putStrLn $ "f(x)の、a = 1 における勾配は、f'(a) = " ++ (show $ differential f 1)
    putStrLn $ "f(x)の、a = -1 における勾配は、f'(a) = " ++ (show $ differential f (-1))
    putStrLn $ "f(x)の、a = 20 における勾配は、f'(a) = " ++ (show $ differential f 20)


-- 微分係数を計算するときにxに与える微小変化
h :: Double
h = 0.00001

-- 勾配法で使う、正の定数
epsilon :: Double
epsilon = 0.00001

-- 微分を計算
differential :: (Double -> Double) -> Double -> Double
differential f a = (f (a + h) - f a) / h

-- 極小点探索（勾配法）
find_min :: (Double -> Double) -> Double -> Double -> Double
find_min f a g =
    let
        a' = a - epsilon * g
        g' = differential f a'
    in
        if g * g' <= 0
            then a
            else find_min f a' g
