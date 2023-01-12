{-# LANGUAGE TupleSections #-}

module Lib.Path 
  ( genRandomPath
  , genRandomPoints
  , genStartEndPoints
  , connectTwoPoints
  , connectAllPoints
  , createAllPaths
  , isValidPath
  , gridifyPath
  , segmentsOverlap
  , picturizePath
  , Path
  , Point
  ) where

import Lib.Grid (gridCols, gridRows, GridArray, emtpyGrid)
import System.Random (randomRIO)
import Data.List (group)
import Lib.Util (cartProd, manhattanDist)
import Graphics.Gloss (Picture)
import GameObjects.Terrain (terrainTiles, drawTerrain, TerrainTiles (roadHorizontal))
import Data.Array ((//))
import Data.Maybe (mapMaybe)

type Point = (Int, Int)
type Path = [Point]

intermediatePointRange :: (Int, Int)
intermediatePointRange = (3, 5)

pathLengthRange :: (Int, Int)
pathLengthRange = (30, 70)

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

pathSegments :: Path -> [(Point, Point)]
pathSegments path = zip path (tail path)

allSegmentPairs :: Path -> [((Point, Point), (Point, Point))]
allSegmentPairs path = filter (uncurry (/=)) $ cartProd allSegments allSegments
  where allSegments = pathSegments path

pathLength :: Path -> Int
pathLength = sum . map (uncurry manhattanDist) . pathSegments

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
isValidPath []   = False
isValidPath [_]  = False
isValidPath path = 
  (not . any (uncurry segmentsOverlap)) (allSegmentPairs path)
  && pathLength path >= fst pathLengthRange
  && pathLength path <= snd pathLengthRange

-- | Turns a path into a grid. The path has to be valid.
gridifyPath :: Path -> GridArray
gridifyPath path = emtpyGrid // pathIndices // turnIndices // crossingIndices
  where 
    pathIndices = concatMap markGrid $ pathSegments path
    markGrid ((x1, y1), (x2, y2))
      | x1 == x2  = [((x1, y), '|') | y <- [min y1 y2 .. max y1 y2]]
      | y1 == y2  = [((x, y1), '_') | x <- [min x1 x2 .. max x1 x2]]
      | otherwise = error "gridifyPath: impossible"
    turnIndices = map (\s@(a, _) -> (a, turnType s)) $ pathSegments path
    turnType ((x1, y1), (x2, y2))
      | x1 == x2  = if y1 < y2 then 'v' else '^'
      | y1 == y2  = if x1 < x2 then '>' else '<'
      | otherwise = error "gridifyPath: impossible"
    crossingIndices = map (,'+') $ mapMaybe (uncurry getCrossing) $ allSegmentPairs path
    -- TODO this is wrong
    getCrossing ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
      | x1 == x2 && y3 == y4 = Just (x1, y3)
      | x3 == x4 && y1 == y2 = Just (x3, y1)
      | otherwise            = Nothing

picturizePath :: Path -> IO Picture
picturizePath path = do
  tTil <- terrainTiles
  return $ drawTerrain $ map (\(x, y) -> (x, y, roadHorizontal tTil)) path

    
-- TODO
genRandomPath :: IO Path
genRandomPath = do
  n <- randomRIO intermediatePointRange
  xs <- genRandomPoints n
  (start, end) <- genStartEndPoints
  return $ start : xs ++ [end]