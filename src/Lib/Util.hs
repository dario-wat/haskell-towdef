{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Lib.Util where

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

manhattanDist :: (Int, Int) -> (Int, Int) -> Int
manhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)