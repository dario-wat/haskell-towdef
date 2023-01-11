{-# LANGUAGE TupleSections #-}

module Lib.Path 
  ( genRandomPath
  , createAllPaths
  , genRandomPoints
  , connectTwoPoints
  , connectAllPoints
  , gridPath
  ) where

import Lib.Grid (gridCols, gridRows, GridArray, emtpyGrid)
import System.Random (randomRIO)
import Data.Array ((//))
import Debug.Trace (traceShowId)

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
connectTwoPoints (x1, y1) (x2, y2) = [pathX1 ++ pathY2, pathY1 ++ pathX2]
  where
    range (i, j)
      | i == j    = []
      | i < j     = [i..j]
      | otherwise = [i,i-1..j]
    xRange = range (x1, x2)
    yRange = range (y1, y2)
    pathX1 = [(x, y1) | x <- xRange]
    pathX2 = [(x, y2) | x <- xRange]
    pathY1 = [(x1, y) | y <- yRange]
    pathY2 = [(x2, y) | y <- yRange]

-- | Creates a list of paths for each adjacent pair of points
connectAllPoints :: [Point] -> [[Path]]
connectAllPoints []         = []
connectAllPoints [_]        = []
connectAllPoints (p1:p2:ps) = connectTwoPoints p1 p2 : connectAllPoints (p2:ps)

-- | Combines all path lists created by connectAllPoints recursively
combinePaths :: [[Path]] -> [Path]
combinePaths = combinePathsAcc []

combinePathsAcc :: [Path] -> [[Path]] -> [Path]
combinePathsAcc acc [] = acc
combinePathsAcc acc ([p]:ps) = combinePathsAcc (concat (p : acc) : acc) ps
combinePathsAcc acc ([p1,p2]:ps)  = combinePathsAcc (concat (p1 : acc) : concat (p2 : acc) : acc) ps
combinePathsAcc _ _ = error "combinePathsAcc: impossible"

createAllPaths :: [Point] -> [Path]
createAllPaths = combinePaths . connectAllPoints

-- TODO
genRandomPath :: IO Path
genRandomPath = do
  n <- randomRIO intermediatePointRange
  xs <- genRandomPoints n
  (start, end) <- genStartEndPoints
  return $ start : xs ++ [end]

gridPath :: Path -> GridArray
gridPath path = emtpyGrid // map (,'X') path