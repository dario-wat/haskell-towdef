module Lib.Util 
  ( cartProd
  , manhattanDist
  , inRangeAbs
  , inRangeAbsExcl
  , inRangeF
  , count
  , chooseRandom
  , chooseRandomReplacement
  , both
  , distance
  , angle
  , headOrDefault
  ) where

import System.Random (randomRIO)
import System.Random.Shuffle (shuffleM)
import Control.Monad (replicateM)

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

chooseRandomReplacement :: Int -> [a] -> IO [a]
chooseRandomReplacement n xs = replicateM n $ (xs !!) <$> randomRIO (0, length xs - 1)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int)

angle :: (Float, Float) -> (Float, Float) -> Float
angle (x1, y1) (x2, y2) = atan2 (y2 - y1) (x2 - x1)

headOrDefault :: a -> [a] -> a
headOrDefault d [] = d
headOrDefault _ (x:_) = x