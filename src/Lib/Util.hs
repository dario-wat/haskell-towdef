module Lib.Util 
  ( cartProd
  , manhattanDist
  , inRangeAbs
  , inRangeAbsExcl
  ) where

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

manhattanDist :: (Int, Int) -> (Int, Int) -> Int
manhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

inRangeAbs :: (Int, Int) -> Int -> Bool
inRangeAbs (a, b) x = x >= min a b && x <= max a b

inRangeAbsExcl :: (Int, Int) -> Int -> Bool
inRangeAbsExcl (a, b) x = x > min a b && x < max a b