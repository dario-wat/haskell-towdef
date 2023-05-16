{-# LANGUAGE TupleSections #-}

module Lib.Level.WaterPathDef 
  ( addWaterToGrid
  ) where

import Data.Array ((//))
import Lib.Level.Grid (Grid(..))
import qualified Lib.Level.Point as P
import qualified Lib.Level.TileType as TT

-- TODO
-- WIP

addWaterToGrid :: Grid -> IO Grid
addWaterToGrid grid = do
  [start, end] <- P.genRandomNPointsWithUnique P.EdgeExcl 2
  let
    getPath :: P.Point -> P.Point -> [P.Point]
    getPath p1@(x1, y1) p2@(x2, y2)
      | P.areOnAdjacentEdgeExcl p1 p2 = [p1, getInterPoint p1 p2, p2]
      | P.areOnOppositeEdgeExcl p1 p2 = p1 : getMiddlePoints p1 p2 ++ [p2]
      | otherwise = error "Points are not on adjacent or opposite edges"
      where
        getInterPoint :: P.Point -> P.Point -> P.Point
        getInterPoint p1@(x1, y1) p2@(x2, y2)
          | P.isOnHorizontalEdge p1 && P.isOnVerticalEdge p2 = (x1, y2)
          | P.isOnVerticalEdge p1 && P.isOnHorizontalEdge p2 = (x2, y1)
          | otherwise = error "impossible: Points are not on adjacent edges"

        getMiddlePoints :: P.Point -> P.Point -> [P.Point]
        getMiddlePoints p1@(x1, y1) p2@(x2, y2) 
          | P.isOnHorizontalEdge p1 && P.isOnHorizontalEdge p2 = 
              [(x1, y1 + (y2 - y1) `div` 2), (x2, y1 + (y2 - y1) `div` 2)]
          | P.isOnVerticalEdge p1 && P.isOnVerticalEdge p2 = 
              [(x1 + (x2 - x1) `div` 2, y1), (x1 + (x2 - x1) `div` 2, y2)]
          | otherwise = error "impossible: Points are not on opposite edges"

  -- let 
  --   emptyTiles = filter ((==TT.Empty) . snd) $ assocs $ unGrid grid
  --   waterTiles = map (,TT.Water) emptyTiles
  -- TODO I was here
  return $ Grid $ unGrid grid // map (,TT.Water) (getPath start end)