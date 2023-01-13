{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Lib.Level.Path 
  ( genRandomPath
  , segmentOverlap
  , segmentCrossing
  , segmentCornerType
  , pathSegments
  , allSegmentPairs
  , cornerChar
  , pathLength
  , Path
  , Point
  ) where

-- TODO WIP

import Lib.Level.Grid (gridCols, gridRows)
import System.Random (randomRIO)
import Data.List (group)
import Lib.Util (cartProd, manhattanDist, inRangeAbsExcl)
import qualified Control.Monad.HT as M (until)
import Data.Ix (Ix(inRange))


type Point = (Int, Int)
type Path = [Point]
type PathSegment = (Point, Point)

data CornerType = UpLeft | UpRight | DownLeft | DownRight
  deriving (Show, Eq)

cornerChar :: CornerType -> Char
cornerChar UpLeft    = '┐'
cornerChar UpRight   = '┌'
cornerChar DownLeft  = '┘'
cornerChar DownRight = '└'

intermediatePointRange :: (Int, Int)
intermediatePointRange = (3, 5)

pathLengthRange :: (Int, Int)
pathLengthRange = (30, 70)

crossingCountRange :: (Int, Int)
crossingCountRange = (0, 2)

-- | Checks whether a path is valid. Validity is a broad term in this case
-- and it also includes path preferences such as path length.
-- Validity requirements:
--    1. Has to have at least two points
--    2. Paths cannot overlap vertically or horizontally, 
--       but can cross perpendicularly
--    3. Path length has to be within the specified range
isValidPath :: Path -> Bool
isValidPath []   = False
isValidPath [_]  = False
isValidPath path = 
  (not . any (uncurry segmentOverlap)) (allSegmentPairs path)
  && inRange pathLengthRange (pathLength path)
  -- && inRange crossingCountRange ()
    
-- | Generates a single random path that satisfies all validity requirements.
genRandomPath :: IO Path
genRandomPath = head <$> M.until (not . null) genRandomPaths
  where
    genRandomPaths = do
      n <- randomRIO intermediatePointRange
      points <- genRandomPoints n
      (start, end) <- genStartEndPoints
      return $ filter isValidPath $ createAllPaths $ start : points ++ [end]

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
combinePathsAcc !acc []           = acc
combinePathsAcc !acc ([p]:ps)     = combinePathsAcc (map (++p) acc) ps
combinePathsAcc !acc ([p1,p2]:ps) = combinePathsAcc (map (++p1) acc ++ map (++p2) acc) ps
combinePathsAcc _   _             = error "combinePathsAcc: impossible"

createAllPaths :: [Point] -> [Path]
createAllPaths = map removeConsecutiveDuplicates . combinePaths . connectAllPoints
  where removeConsecutiveDuplicates = map head . group

pathSegments :: Path -> [PathSegment]
pathSegments path = zip path (tail path)

allSegmentPairs :: Path -> [(PathSegment, PathSegment)]
allSegmentPairs path = filter (uncurry (/=)) $ cartProd allSegments allSegments
  where allSegments = pathSegments path

pathLength :: Path -> Int
pathLength = (+1) . sum . map (uncurry manhattanDist) . pathSegments

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
segmentCornerType :: PathSegment -> PathSegment -> Maybe (Point, CornerType)
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
      | x1 < x2  && y3 > y2  = Just (p, UpLeft)
      | x1 < x2  && y3 < y2  = Just (p, DownLeft)
      | x1 == x2 && y1 < y2  = Just (p, DownRight)
      | x1 == x2 && y1 > y2  = Just (p, UpRight)
      | otherwise            = Nothing