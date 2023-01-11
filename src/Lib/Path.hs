{-# LANGUAGE TupleSections #-}

module Lib.Path 
  ( genRandomPath
  , genRandomPoints
  , genStartEndPoints
  , connectTwoPoints
  , connectAllPoints
  , createAllPaths
  , isValidPath
  , segmentsOverlap
  , Path
  , Point
  ) where

import Lib.Grid (gridCols, gridRows)
import System.Random (randomRIO)
import Data.List (group)

type Point = (Int, Int)
type Path = [Point]

intermediatePointRange :: (Int, Int)
intermediatePointRange = (3, 5)

genRandomPoint :: IO Point
genRandomPoint = do
  x <- randomRIO (0, gridCols - 1)
  y <- randomRIO (0, gridRows - 1)
  return (x, y)

-- | Generates a list of n unique random grid points
genRandomPoints :: Int -> IO [Point]
genRandomPoints n = genMorePoints []
  where
    genMorePoints curr
      | length curr == n = return curr
      | otherwise        = do
        p <- genRandomPoint
        if p `elem` curr then genMorePoints curr else genMorePoints (p : curr)

genRandomEdgePoint :: IO Point
genRandomEdgePoint = do
  edge <- randomRIO (0 :: Int, 3)
  case edge of
    0 -> (,0) <$> randomRIO (0, gridCols - 1)
    1 -> (, gridRows-1) <$> randomRIO (0, gridCols - 1)
    2 -> (0,) <$> randomRIO (0, gridRows - 1)
    3 -> (gridCols-1,) <$> randomRIO (0, gridRows - 1)
    _ -> error "genRandomEdgePoint: impossible"
  
genStartEndPoints :: IO (Point, Point)
genStartEndPoints = do
  start <- genRandomEdgePoint
  end <- genRandomEdgePoint
  if start == end then genStartEndPoints else return (start, end)

-- | There are either one or two paths between two points.
-- There is only one path if the points are on the same row or column.
-- Otherwise there are two paths, one going up and one going right.
connectTwoPoints :: Point -> Point -> [Path]
connectTwoPoints (x1, y1) (x2, y2)
  | x1 == x2 || y1 == y2 = [singlePath]
  | otherwise            = [path1, path2]
  where
    path1 = [(x1, y1), (x1, y2), (x2, y2)]
    path2 = [(x1, y1), (x2, y1), (x2, y2)]
    singlePath = [(x1, y1), (x2, y2)]

-- | Creates a list of paths for each adjacent pair of points
connectAllPoints :: [Point] -> [[Path]]
connectAllPoints []         = []
connectAllPoints [_]        = []
connectAllPoints (p1:p2:ps) = connectTwoPoints p1 p2 : connectAllPoints (p2:ps)

-- -- | Combines all path lists created by connectAllPoints recursively
combinePaths :: [[Path]] -> [Path]
combinePaths = combinePathsAcc [[]]

combinePathsAcc :: [Path] -> [[Path]] -> [Path]
combinePathsAcc acc []           = acc
combinePathsAcc acc ([p]:ps)     = combinePathsAcc (map (++p) acc) ps
combinePathsAcc acc ([p1,p2]:ps) = combinePathsAcc (map (++p1) acc ++ map (++p2) acc) ps
combinePathsAcc _   _            = error "combinePathsAcc: impossible"

createAllPaths :: [Point] -> [Path]
createAllPaths = map removeConsecutiveDuplicates . combinePaths . connectAllPoints
  where removeConsecutiveDuplicates = map head . group

-- | Checks whether two path segments overlap
segmentsOverlap :: (Point, Point) -> (Point, Point) -> Bool
segmentsOverlap ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
  | all (== x1) [x2, x3, x4] = overlap y1 y2 y3 y4
  | all (== y1) [y2, y3, y4] = overlap x1 x2 x3 x4
  | otherwise                = False
  where 
    overlap a1 a2 b1 b2
      | a1 > a2   = overlap a2 a1 b1 b2
      | b1 > b2   = overlap a1 a2 b2 b1
      | a1 > b1   = overlap b1 b2 a1 a2
      | a2 <= b1  = False
      | otherwise = True

-- | Checks whether a path is valid. That means it needs to satisfy 
-- the following conditions:
--    1. Has to have at least two points
--    2. Paths cannot overlap vertically or horizontally, 
--       but can cross perpendicularly
--    3. 
isValidPath :: Path -> Bool
isValidPath [] = False
isValidPath [_] = False
isValidPath path = (not . any (uncurry segmentsOverlap)) allSegmentPairs
  where
    allSegments = zip path (tail path)
    allSegmentPairs = filter (uncurry (/=)) $ cartProd allSegments allSegments
    cartProd xs ys = [(x,y) | x <- xs, y <- ys]
    

-- TODO
genRandomPath :: IO Path
genRandomPath = do
  n <- randomRIO intermediatePointRange
  xs <- genRandomPoints n
  (start, end) <- genStartEndPoints
  return $ start : xs ++ [end]