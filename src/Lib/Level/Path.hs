{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Lib.Level.Path 
  ( Path      -- TODO this shouldnt be exported
  , genRandomPath
  , addPathToGrid
  , toGlossPath
  ) where

-- TODO
-- 1. Make this more general so that it can be reused for water

import Prelude hiding (Left, Right)
import System.Random (randomRIO)
import Data.Array ((//))
import Data.Bifunctor ( Bifunctor(second) )
import Data.Ix (Ix(inRange))
import Data.List (group, nub)
import Data.Maybe (mapMaybe, isJust)
import qualified Control.Monad.HT as M (until)
import qualified Graphics.Gloss as GL
import qualified Lib.Level.Grid as G
import qualified Lib.Level.PathSegment as PS
import qualified Lib.Level.Point as P
import Lib.Util (cartProd, manhattanDist, count)
import qualified Lib.Level.TileType as TT
import Lib.Level.Point (PointLocation(EdgeExcl))

type Path = [P.Point]


intermediatePointRange :: (Int, Int)
intermediatePointRange = (3, 5)

pathLengthRange :: (Int, Int)
pathLengthRange = (70, 110)

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
--    7. Path segments cannot be adjacent
isValidPath :: Path -> Bool
isValidPath []   = False
isValidPath [_]  = False
isValidPath path = 
  hasNoOverlap path
  && inRange pathLengthRange (pathLength path)
  && inRange crossingCountRange (pathCrossingCount path)
  && (not . any PS.isEdge) (pathSegments path)
  && pathQuadrantCount path >= 3
  && hasNoAdjacentSegments path
  where
    hasNoOverlap = not . pathHasOverlap
    hasNoAdjacentSegments = not . pathHasAdjacentSegment
    
-- | Generates a single random path that satisfies all validity requirements.
genRandomPath :: IO Path
genRandomPath = head <$> M.until (not . null) genRandomPaths
  where
    genRandomPaths = do
      n <- randomRIO intermediatePointRange
      points <- P.genRandomNPointsOn P.Center n
      (start, end) <- genStartEndPoints
      return $ filter isValidPath $ createAllPaths $ start : points ++ [end]

addPathToGrid :: G.Grid -> Path -> G.Grid
addPathToGrid grid path = G.Grid $ G.unGrid grid // pathIndices // turnIndices // crossingIndices
  where 
    pathIndices = concatMap markGridRoads $ pathSegments path
    markGridRoads ((x1, y1), (x2, y2))
      | x1 == x2  = [((x1, y), TT.RoadVertical)   | y <- [min y1 y2 .. max y1 y2]]
      | y1 == y2  = [((x, y1), TT.RoadHorizontal) | x <- [min x1 x2 .. max x1 x2]]
      | otherwise = error "markGridRoads: impossible"
    turnIndices = mapMaybe roadType $ allSegmentPairs path
    crossingIndices = 
      map (,TT.RoadCrossing) $ mapMaybe (uncurry PS.crossing) $ allSegmentPairs path
    roadType = (second cornerTypeToTileType <$>) . uncurry PS.cornerType
    cornerTypeToTileType PS.TopLeft = TT.RoadUpRight
    cornerTypeToTileType PS.TopRight = TT.RoadUpLeft
    cornerTypeToTileType PS.BottomLeft = TT.RoadDownRight
    cornerTypeToTileType PS.BottomRight = TT.RoadDownLeft

toGlossPath :: Path -> GL.Path
toGlossPath = map (`G.gridCenterOf` (1, 1))

-------------------------------------------------------------------------------
-- Path lib
-------------------------------------------------------------------------------

pathHasAdjacentSegment :: Path -> Bool
pathHasAdjacentSegment = any (uncurry PS.areAdjacent) . allSegmentPairs

pathLength :: Path -> Int
pathLength = (+1) . sum . map (uncurry manhattanDist) . pathSegments

pathHasOverlap :: Path -> Bool
pathHasOverlap = any (uncurry PS.haveOverlap) . allSegmentPairs

pathCrossingCount :: Path -> Int
pathCrossingCount = count (isJust . uncurry PS.crossing) . allSegmentPairs

pathQuadrantCount :: Path -> Int
pathQuadrantCount = length . nub . map P.quadrant


-------------------------------------------------------------------------------
-- Path creation
-------------------------------------------------------------------------------

-- | Generates two random points on the edges of the grid, excluding corners
genStartEndPoints :: IO (P.Point, P.Point)
genStartEndPoints = do
  start <- P.genRandomPointWith EdgeExcl
  end <- P.genRandomPointWith EdgeExcl
  if start == end then genStartEndPoints else return (start, end)

-- | There are either one or two paths between two points.
-- There is only one path if the points are on the same row or column.
-- Otherwise there are two paths, one going up and one going right.
connectTwoPoints :: P.Point -> P.Point -> [Path]
connectTwoPoints (x1, y1) (x2, y2)
  | x1 == x2 || y1 == y2 = [singlePath]
  | otherwise            = [path1, path2]
  where
    path1 = [(x1, y1), (x1, y2), (x2, y2)]
    path2 = [(x1, y1), (x2, y1), (x2, y2)]
    singlePath = [(x1, y1), (x2, y2)]

-- | Creates a list of paths for each adjacent pair of points
connectAllPoints :: [P.Point] -> [[Path]]
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

createAllPaths :: [P.Point] -> [Path]
createAllPaths = map removeConsecutiveDuplicates . combinePaths . connectAllPoints
  where removeConsecutiveDuplicates = map head . group


-------------------------------------------------------------------------------
-- Path segments
-------------------------------------------------------------------------------

pathSegments :: Path -> [PS.PathSegment]
pathSegments path = zip path (tail path)

allSegmentPairs :: Path -> [(PS.PathSegment, PS.PathSegment)]
allSegmentPairs path = filter (uncurry (/=)) $ cartProd allSegments allSegments
  where allSegments = pathSegments path