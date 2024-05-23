module Main (main) where

import Lib

main :: IO ()
main = print $ listSum [1, 2, 3, 4, 5]

listSum :: [Int] -> Int
listSum [] = 0
listSum (x:xs) = x + listSum xs
