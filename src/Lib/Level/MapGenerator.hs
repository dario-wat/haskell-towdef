{-# LANGUAGE TupleSections #-}

module Lib.Level.MapGenerator 
  ( addPathToGrid
  , picturizePath
  ) where

import Data.Array ((//))
import Data.Bifunctor (Bifunctor(second))
import Data.Maybe (mapMaybe)
import qualified Graphics.Gloss as G
import Lib.Level.Path (Path, pathSegments, allSegmentPairs, segmentCornerType, segmentCrossing, cornerChar)
import Lib.Level.Grid (Grid(..))
import GameObjects.Terrain (terrainTiles, drawTerrain, Terrain (Terrain), TerrainTiles(..))

-- | Turns a path into a grid. The path has to be valid.
addPathToGrid :: Grid -> Path -> Grid
addPathToGrid grid path = Grid $ unGrid grid // pathIndices // turnIndices // crossingIndices
  where 
    pathIndices = concatMap markGrid $ pathSegments path
    markGrid ((x1, y1), (x2, y2))
      | x1 == x2  = [((x1, y), '|') | y <- [min y1 y2 .. max y1 y2]]
      | y1 == y2  = [((x, y1), '_') | x <- [min x1 x2 .. max x1 x2]]
      | otherwise = error "gridifyPath: impossible"
    turnIndices = map (second cornerChar) $ mapMaybe (uncurry segmentCornerType) $ allSegmentPairs path
    crossingIndices = map (,'+') $ mapMaybe (uncurry segmentCrossing) $ allSegmentPairs path

picturizePath :: Path -> IO G.Picture
picturizePath path = do
  tTil <- terrainTiles
  return $ drawTerrain $ Terrain $ map (\(x, y) -> (x, y, roadHorizontal tTil)) path
