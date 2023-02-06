{-# LANGUAGE TupleSections #-}

module Lib.Level.Point 
  ( Point
  , Edge(..)
  , EdgeExcl(..)
  , Corner(..)
  , PointLocation(..)
  , genRandomPointOn
  , genRandomNPointsOn
  , genRandomPointsOn
  , genStartEndPoints
  , quadrant
  , isCorner
  , isEdge
  ) where

import Prelude hiding (Left, Right)
import System.Random (randomRIO)
import Lib.Level.Grid (gridCols, gridRows)

type Point = (Int, Int)

-- | Edge points, including corner points
data Edge = Top | Bottom | Left | Right 
  deriving (Show, Eq, Enum, Bounded)

-- | Edge points, excluding corner points
data EdgeExcl = TopExcl | BottomExcl | LeftExcl | RightExcl 
  deriving (Show, Eq, Enum, Bounded)

-- | Corner points
data Corner = TopLeft | TopRight | BottomLeft | BottomRight 
  deriving (Show, Eq, Enum, Bounded)

-- | Point location
data PointLocation = 
  Edge Edge 
  | EdgeExcl EdgeExcl 
  | Corner Corner 
  | Center -- Excluding edges and corners
  | Any    -- Any point on the grid
  deriving (Show, Eq)

-- | Genereates a random point on the grid given the point location.
genRandomPointOn :: PointLocation -> IO Point
genRandomPointOn (Edge Top)            = (,0)          <$> randomRIO (0, gridCols - 1)
genRandomPointOn (Edge Bottom)         = (,gridRows-1) <$> randomRIO (0, gridCols - 1)
genRandomPointOn (Edge Left)           = (0,)          <$> randomRIO (0, gridRows - 1)
genRandomPointOn (Edge Right)          = (gridCols-1,) <$> randomRIO (0, gridRows - 1)
genRandomPointOn (EdgeExcl TopExcl)    = (,1)          <$> randomRIO (1, gridCols - 2)
genRandomPointOn (EdgeExcl BottomExcl) = (,gridRows-2) <$> randomRIO (1, gridCols - 2)
genRandomPointOn (EdgeExcl LeftExcl)   = (1,)          <$> randomRIO (1, gridRows - 2)
genRandomPointOn (EdgeExcl RightExcl)  = (gridCols-2,) <$> randomRIO (1, gridRows - 2)
genRandomPointOn (Corner TopLeft)      = return (0, 0)
genRandomPointOn (Corner TopRight)     = return (gridCols-1, 0)
genRandomPointOn (Corner BottomLeft)   = return (0, gridRows-1)
genRandomPointOn (Corner BottomRight)  = return (gridCols-1, gridRows-1)
genRandomPointOn Center                = (,) <$> randomRIO (1, gridCols - 2) <*> randomRIO (1, gridRows - 2)
genRandomPointOn Any                   = (,) <$> randomRIO (0, gridCols - 1) <*> randomRIO (0, gridRows - 1)

-- | Generates a list of n unique random grid points
genRandomNPointsOn :: PointLocation -> Int -> IO [Point]
genRandomNPointsOn pLoc n = genMorePoints []
  where
    genMorePoints curr
      | length curr == n = return curr
      | otherwise        = do
        p <- genRandomPointOn pLoc
        if p `elem` curr then genMorePoints curr else genMorePoints (p : curr)

genRandomPointsOn :: [PointLocation] -> IO [Point]
genRandomPointsOn = mapM genRandomPointOn

genRandomEdgeExclPoint :: IO Point
genRandomEdgeExclPoint = 
  genRandomPointOn . Edge . ([minBound..maxBound] !!) =<< randomRIO (0 :: Int, 3)

-- | Generates two random points on the edges of the grid, excluding corners
genStartEndPoints :: IO (Point, Point)
genStartEndPoints = do
  start <- genRandomEdgeExclPoint
  end <- genRandomEdgeExclPoint
  if start == end then genStartEndPoints else return (start, end)

-- | Returns the quadrant of a point on the grid as an integer
quadrant :: Point -> Int
quadrant (x, y)
  | x <  gridCols `div` 2 && y <  gridRows `div` 2 = 1
  | x <  gridCols `div` 2 && y >= gridRows `div` 2 = 2
  | x >= gridCols `div` 2 && y <  gridRows `div` 2 = 3
  | x >= gridCols `div` 2 && y >= gridRows `div` 2 = 4
  | otherwise = error "quadrant: impossible"

isEdge :: Point -> Bool
isEdge (x, y) = x == 0 || x == gridCols - 1 || y == 0 || y == gridRows - 1

isCorner :: Point -> Bool
isCorner (x, y) = (x == 0 || x == gridCols - 1) && (y == 0 || y == gridRows - 1)

