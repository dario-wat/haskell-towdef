module Lib.Util 
  ( cartProd
  , manhattanDist
  , inRangeAbs
  , inRangeAbsExcl
  , inRangeF
  , count
  , chooseRandom
  ) where
import System.Random.Shuffle (shuffleM)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

manhattanDist :: (Int, Int) -> (Int, Int) -> Int
manhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

inRangeAbs :: (Int, Int) -> Int -> Bool
inRangeAbs (a, b) x = x >= min a b && x <= max a b

inRangeAbsExcl :: (Int, Int) -> Int -> Bool
inRangeAbsExcl (a, b) x = x > min a b && x < max a b

inRangeF :: (Float, Float) -> Float -> Bool
inRangeF (a, b) x = x >= min a b && x <= max a b

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

chooseRandom :: Int -> [a] -> IO [a]
chooseRandom n xs = take n <$> shuffleM xs