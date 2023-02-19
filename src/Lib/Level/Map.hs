{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Lib.Level.Map
  ( Map(..) 
  , update
  , draw
  , generateMap
  ) where

import System.Random (randomRIO)
import Data.Array (assocs, (//))
import qualified Graphics.Gloss as G
import Lib.Level.Grid (Grid(..), emptyGrid)
import Lib.Level.Path (Path, genRandomPath, addPathToGrid)
-- import Lib.Level.Point (genStartEndPoints)
import qualified Lib.Level.TileType as TT
import Lib.Util (chooseRandom, chooseRandomReplacement)
import qualified GameObjects.Sprite as S
import qualified GameObjects.Terrain as T
import Graphics.Gloss (pictures)
import qualified Lib.Level.Point as P

-- TODO
-- WIP
-- Make corners rounded
-- Water generate and draw

data Map = Map
  { grid  :: !Grid
  , path  :: !Path
  , tiles :: ![S.Sprite]
  }

greenTreeRange :: (Int, Int)
greenTreeRange = (8, 12)

brownTreeRange :: (Int, Int)
brownTreeRange = (7, 10)

rockRange :: (Int, Int)
rockRange = (6, 8)

bushRange :: (Int, Int)
bushRange = (4, 6)

objectsToAdd :: IO [TT.TileType]
objectsToAdd = concat <$> sequence
  [ flip chooseRandomReplacement TT.greenTrees =<< randomRIO greenTreeRange
  , flip chooseRandomReplacement TT.brownTrees =<< randomRIO brownTreeRange
  , flip chooseRandomReplacement TT.rocks      =<< randomRIO rockRange
  , flip chooseRandomReplacement TT.bushes     =<< randomRIO bushRange
  ]

addObjectsToGrid :: Grid -> IO Grid
addObjectsToGrid grid = do
  let emptyTiles = filter ((==TT.Empty) . snd) $ assocs $ unGrid grid
  objs <- objectsToAdd
  let totalCount = length objs
  shuffledIndices <- map fst <$> chooseRandom totalCount emptyTiles
  return $ Grid $ unGrid grid // zip shuffledIndices objs

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

-- | Creates a list of sprites given a grid and a function that maps a
-- | TileType to a Tile.
spritifyTiles :: (TT.TileType -> T.Tile) -> Grid -> [S.Sprite]
spritifyTiles tf = map (T.mkSpriteFromTile . tTypeF) . assocs . unGrid
  where tTypeF ((x, y), tType) = (x, y, tf tType)

generateMap :: IO Map
generateMap = do
  tTil <- T.terrainTiles
  tObj <- T.terrainObjects
  tWtr <- T.waterTiles
  let
    tileMap :: TT.TileType -> T.Tile
    tileMap TT.Empty          = T.grass tTil
    tileMap TT.RoadHorizontal = T.roadHorizontal tTil
    tileMap TT.RoadVertical   = T.roadVertical tTil
    tileMap TT.RoadUpLeft     = T.roadBottomRightSharp tTil
    tileMap TT.RoadUpRight    = T.roadBottomLeftSharp tTil
    tileMap TT.RoadDownLeft   = T.roadTopRightSharp tTil
    tileMap TT.RoadDownRight  = T.roadTopLeftSharp tTil
    tileMap TT.RoadCrossing   = T.roadCrossing tTil
    tileMap TT.Grass          = T.grass tTil
    tileMap TT.GreenTree1     = T.greenTree1 tObj
    tileMap TT.GreenTree2     = T.greenTree2 tObj
    tileMap TT.GreenTree3     = T.greenTree3 tObj
    tileMap TT.GreenTree4     = T.greenTree4 tObj
    tileMap TT.BrownTree1     = T.brownTree1 tObj
    tileMap TT.BrownTree2     = T.brownTree2 tObj
    tileMap TT.BrownTree3     = T.brownTree3 tObj
    tileMap TT.BrownTree4     = T.brownTree4 tObj
    tileMap TT.Rock1          = T.rock1 tObj
    tileMap TT.Rock2          = T.rock2 tObj
    tileMap TT.Rock3          = T.rock3 tObj
    tileMap TT.Rock4          = T.rock4 tObj
    tileMap TT.Bush1          = T.bush1 tObj
    tileMap TT.Bush2          = T.bush2 tObj
    tileMap TT.Water          = T.fullWave1 tWtr

    grassBackground = spritifyTiles (const $ T.grass tTil) emptyGrid
  path <- genRandomPath
  grid <- addWaterToGrid =<< addObjectsToGrid (addPathToGrid emptyGrid path)
  return $ Map grid path $ grassBackground ++ spritifyTiles tileMap grid

draw :: Float -> Map -> G.Picture
draw time Map{tiles} = pictures $ map (S.draw time) tiles
    
update :: Float -> Map -> Map
update time lmap@Map{tiles} = lmap { tiles = map (S.update time) tiles }