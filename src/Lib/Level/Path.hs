{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Lib.Level.Path 
  ( genRandomPath
  , addPathToGrid
  ) where

import System.Random (randomRIO)
import Data.Array ((//))
import Data.Ix (Ix(inRange))
import Data.List (group, nub)
import Data.Maybe (mapMaybe, isJust)
import qualified Control.Monad.HT as M (until)
import Lib.Level.Grid (gridCols, gridRows, Grid(..))
import Lib.Util (cartProd, manhattanDist, inRangeAbsExcl, count)
import qualified Lib.Level.TileType as TT

type Point = (Int, Int)
type Path = [Point]
type PathSegment = (Point, Point)

-- TODO
-- not adjacent
-- not cross at the edge
-- not go across edges
-- crossing doesnt work 100%

intermediatePointRange :: (Int, Int)
intermediatePointRange = (3, 5)

pathLengthRange :: (Int, Int)
pathLengthRange = (50, 110)

crossingCountRange :: (Int, Int)
crossingCountRange = (0, 2)

-- | Checks whether a path is valid. Validity is a broad term in this case
-- and it also includes path preferences such as path length.
-- Validity requirements:
--    1. Has to have at least two points
--    2. Paths cannot overlap vertically or horizontally, 
--       but can cross perpendicularly
--    3. Path length has to be within the specified range
--    4. Path cannot have too many crossings
--    5. Path cannot be on any of the edges
--    6. Path has to be a part of at least 3 quadrants
isValidPath :: Path -> Bool
isValidPath []   = False
isValidPath [_]  = False
isValidPath path = 
  hasNoOverlap path
  && inRange pathLengthRange (pathLength path)
  && inRange crossingCountRange (crossingCount path)
  && (not . any segmentIsEdge) (pathSegments path)
  && quadrantCount path >= 3
  where
    hasNoOverlap = not . any (uncurry segmentOverlap) . allSegmentPairs
    crossingCount = count (isJust . uncurry segmentCrossing) . allSegmentPairs
    pathLength = (+1) . sum . map (uncurry manhattanDist) . pathSegments
    quadrantCount = length . nub . map pointQuadrant
    
-- | Generates a single random path that satisfies all validity requirements.
genRandomPath :: IO Path
genRandomPath = head <$> M.until (not . null) genRandomPaths
  where
    genRandomPaths = do
      n <- randomRIO intermediatePointRange
      points <- genRandomPoints n
      (start, end) <- genStartEndPoints
      return $ filter isValidPath $ createAllPaths $ start : points ++ [end]

addPathToGrid :: Grid -> Path -> Grid
addPathToGrid grid path = Grid $ unGrid grid // pathIndices // turnIndices // crossingIndices
  where 
    pathIndices = concatMap markGridRoads $ pathSegments path
    markGridRoads ((x1, y1), (x2, y2))
      | x1 == x2  = [((x1, y), TT.RoadVertical) | y <- [min y1 y2 .. max y1 y2]]
      | y1 == y2  = [((x, y1), TT.RoadHorizontal) | x <- [min x1 x2 .. max x1 x2]]
      | otherwise = error "gridifyPath: impossible"
    turnIndices = mapMaybe (uncurry segmentCornerType) $ allSegmentPairs path
    crossingIndices = 
      map (,TT.RoadCrossing) $ mapMaybe (uncurry segmentCrossing) $ allSegmentPairs path


-------------------------------------------------------------------------------
-- Point
-------------------------------------------------------------------------------

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

pointQuadrant :: Point -> Int
pointQuadrant (x, y)
  | x < gridCols `div` 2 && y < gridRows `div` 2 = 1
  | x < gridCols `div` 2 && y >= gridRows `div` 2 = 2
  | x >= gridCols `div` 2 && y < gridRows `div` 2 = 3
  | x >= gridCols `div` 2 && y >= gridRows `div` 2 = 4
  | otherwise = error "pointQuadrant: impossible"

-------------------------------------------------------------------------------
-- Path creation
-------------------------------------------------------------------------------

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
combinePathsAcc !acc []           = acc
combinePathsAcc !acc ([p]:ps)     = combinePathsAcc (map (++p) acc) ps
combinePathsAcc !acc ([p1,p2]:ps) = combinePathsAcc (map (++p1) acc ++ map (++p2) acc) ps
combinePathsAcc _   _             = error "combinePathsAcc: impossible"

createAllPaths :: [Point] -> [Path]
createAllPaths = map removeConsecutiveDuplicates . combinePaths . connectAllPoints
  where removeConsecutiveDuplicates = map head . group


-------------------------------------------------------------------------------
-- Path segments
-------------------------------------------------------------------------------

pathSegments :: Path -> [PathSegment]
pathSegments path = zip path (tail path)

allSegmentPairs :: Path -> [(PathSegment, PathSegment)]
allSegmentPairs path = filter (uncurry (/=)) $ cartProd allSegments allSegments
  where allSegments = pathSegments path

-- | Checks whether two path segments overlap
segmentOverlap :: PathSegment -> PathSegment -> Bool
segmentOverlap ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
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

-- | Finds a crossing point between two segments if there is one
segmentCrossing :: PathSegment -> PathSegment -> Maybe Point
segmentCrossing ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
  | x1 == x2 && y3 == y4 && inRangeAbsExcl (x3, x4) x1 && inRangeAbsExcl (y1, y2) y3 = Just (x1, y3)
  | x3 == x4 && y1 == y2 && inRangeAbsExcl (x1, x2) x3 && inRangeAbsExcl (y3, y4) y1 = Just (x3, y1)
  | otherwise = Nothing

-- | Finds a corner between two segments if there is one
segmentCornerType :: PathSegment -> PathSegment -> Maybe (Point, TT.TileType)
segmentCornerType (a1, b1) (a2, b2)
  | a1 == a2 && b1 == b2 || a1 == b2 && b1 == a2 = Nothing
  | a1 == a2 = getCornerType b1 a1 b2
  | a1 == b2 = getCornerType b1 a1 a2
  | b1 == a2 = getCornerType a1 b1 b2
  | b1 == b2 = getCornerType a1 b1 a2
  | otherwise = Nothing
  where
    getCornerType (x1, y1) p@(x2, y2) (x3, y3)
      | x1 == x3 || y1 == y3 = Nothing    -- Not a corner (straight line)
      | x1 > x3              = getCornerType (x3, y3) (x2, y2) (x1, y1)
      | x1 < x2  && y3 > y2  = Just (p, TT.RoadUpLeft)
      | x1 < x2  && y3 < y2  = Just (p, TT.RoadDownLeft)
      | x1 == x2 && y1 < y2  = Just (p, TT.RoadDownRight)
      | x1 == x2 && y1 > y2  = Just (p, TT.RoadUpRight)
      | otherwise            = Nothing

segmentIsEdge :: PathSegment -> Bool
segmentIsEdge ((x1, y1), (x2, y2)) = isVerticalEdge || isHorizontalEdge
  where
    isVerticalEdge = x1 == x2 && (x1 == 0 || x1 == gridCols - 1)
    isHorizontalEdge = y1 == y2 && (y1 == 0 || y1 == gridRows - 1)