{-# LANGUAGE TupleSections #-}

module Lib.Level.EnemyPathDef 
  ( genRandomPath
  , addPathToGrid
  ) where

import Prelude hiding (Left, Right)
import System.Random (randomRIO)
import Data.Array ((//))
import Data.Bifunctor ( Bifunctor(second) )
import Data.Ix (Ix(inRange))
import Data.Maybe (mapMaybe)
import qualified Control.Monad.HT as M (until)
import qualified Lib.Level.Grid as G
import qualified Lib.Level.Path as P
import qualified Lib.Level.PathSegment as S
import qualified Lib.Level.Point as P
import qualified Lib.Level.TileType as TT

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
isValidPath :: P.Path -> Bool
isValidPath []   = False
isValidPath [_]  = False
isValidPath path = 
  hasNoOverlap path
  && inRange pathLengthRange (P.pathLength path)
  && inRange crossingCountRange (P.crossingCount path)
  && (not . any S.isEdge) (P.pathSegments path)
  && P.quadrantCount path >= 3
  && hasNoAdjacentSegments path
  where
    hasNoOverlap = not . P.hasOverlap
    hasNoAdjacentSegments = not . P.hasAdjacentSegment
    
-- | Generates a single random path that satisfies all validity requirements.
genRandomPath :: IO P.Path
genRandomPath = head <$> M.until (not . null) genRandomPaths
  where
    genRandomPaths = do
      n <- randomRIO intermediatePointRange
      points <- P.genRandomNPointsOn P.Center n
      (start, end) <- genStartEndPoints
      return $ filter isValidPath $ P.createAllPaths $ start : points ++ [end]

addPathToGrid :: G.Grid -> P.Path -> G.Grid
addPathToGrid grid path = G.Grid $ G.unGrid grid // pathIndices // turnIndices // crossingIndices
  where 
    pathIndices = concatMap markGridRoads $ P.pathSegments path
    markGridRoads ((x1, y1), (x2, y2))
      | x1 == x2  = [((x1, y), TT.RoadVertical)   | y <- [min y1 y2 .. max y1 y2]]
      | y1 == y2  = [((x, y1), TT.RoadHorizontal) | x <- [min x1 x2 .. max x1 x2]]
      | otherwise = error "markGridRoads: impossible"
    turnIndices = mapMaybe roadType $ P.allSegmentPairs path
    crossingIndices = 
      map (,TT.RoadCrossing) $ mapMaybe (uncurry S.crossing) $ P.allSegmentPairs path
    roadType = (second cornerTypeToTileType <$>) . uncurry S.cornerType
    cornerTypeToTileType S.TopLeft = TT.RoadUpRight
    cornerTypeToTileType S.TopRight = TT.RoadUpLeft
    cornerTypeToTileType S.BottomLeft = TT.RoadDownRight
    cornerTypeToTileType S.BottomRight = TT.RoadDownLeft

-- | Generates two random points on the edges of the grid, excluding corners
genStartEndPoints :: IO (P.Point, P.Point)
genStartEndPoints = do
  start <- P.genRandomPointWith P.EdgeExcl
  end <- P.genRandomPointWith P.EdgeExcl
  if start == end then genStartEndPoints else return (start, end)