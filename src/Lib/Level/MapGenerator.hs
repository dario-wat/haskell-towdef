{-# LANGUAGE TupleSections #-}

module Lib.Level.MapGenerator 
  ( Map(..) 
  , picturizeGrid
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
import qualified GameObjects.Terrain as T
import Graphics.Gloss (pictures)
import qualified Lib.Level.Point as P

-- TODO
-- WIP
-- Make corners rounded
-- Make enemies follow the path
-- Water generate and draw
-- Use sprites for tiles
-- Might need draw + update

data Map = Map
  { grid :: Grid
  , path :: Path
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
objectsToAdd = do
  grTreeCount <- randomRIO greenTreeRange
  brTreeCount <- randomRIO brownTreeRange
  rockCount   <- randomRIO rockRange
  bushCount   <- randomRIO bushRange
  concat <$> sequence
    [ chooseRandomReplacement grTreeCount TT.greenTrees
    , chooseRandomReplacement brTreeCount TT.brownTrees
    , chooseRandomReplacement rockCount   TT.rocks
    , chooseRandomReplacement bushCount   TT.bushes
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

drawTerraineWithMap :: (TT.TileType -> IO T.Tile) -> Grid -> IO G.Picture
drawTerraineWithMap tf grid = T.drawTerrain . T.Terrain <$> mapM tTypeF (assocs $ unGrid grid)
  where tTypeF ((x, y), tType) = (x, y,) <$> tf tType

generateMap :: IO Map
generateMap = do
  path <- genRandomPath
  grid <- addWaterToGrid =<< addObjectsToGrid (addPathToGrid emptyGrid path)
  return $ Map grid path

picturizeGrid :: Grid -> IO G.Picture
picturizeGrid grid = do
  tTil <- T.terrainTiles
  tObj <- T.terrainObjects
  let 
    randomTile :: [T.Tile] -> IO T.Tile
    randomTile ts = head <$> chooseRandom 1 ts

    grassBackground :: IO G.Picture
    grassBackground = drawTerraineWithMap (const $ return $ T.grass tTil) emptyGrid

    tileMapM :: TT.TileType -> IO T.Tile
    tileMapM TT.Empty          = return $ T.grass tTil
    tileMapM TT.RoadHorizontal = return $ T.roadHorizontal tTil
    tileMapM TT.RoadVertical   = return $ T.roadVertical tTil
    tileMapM TT.RoadUpLeft     = return $ T.roadBottomRightSharp tTil
    tileMapM TT.RoadUpRight    = return $ T.roadBottomLeftSharp tTil
    tileMapM TT.RoadDownLeft   = return $ T.roadTopRightSharp tTil
    tileMapM TT.RoadDownRight  = return $ T.roadTopLeftSharp tTil
    tileMapM TT.RoadCrossing   = return $ T.roadCrossing tTil
    tileMapM TT.Grass          = return $ T.grass tTil
    tileMapM TT.GreenTree1     = return $ T.greenTree1 tObj
    tileMapM TT.GreenTree2     = return $ T.greenTree2 tObj
    tileMapM TT.GreenTree3     = return $ T.greenTree3 tObj
    tileMapM TT.GreenTree4     = return $ T.greenTree4 tObj
    tileMapM TT.BrownTree1     = return $ T.brownTree1 tObj
    tileMapM TT.BrownTree2     = return $ T.brownTree2 tObj
    tileMapM TT.BrownTree3     = return $ T.brownTree3 tObj
    tileMapM TT.BrownTree4     = return $ T.brownTree4 tObj
    tileMapM TT.Rock1          = return $ T.rock1 tObj
    tileMapM TT.Rock2          = return $ T.rock2 tObj
    tileMapM TT.Rock3          = return $ T.rock3 tObj
    tileMapM TT.Rock4          = return $ T.rock4 tObj
    tileMapM TT.Bush1          = return $ T.bush1 tObj
    tileMapM TT.Bush2          = return $ T.bush2 tObj

    tileMapM TT.Water          = return $ T.roadCrossing tTil

  grassBg <- grassBackground
  terrainPic <- drawTerraineWithMap tileMapM grid
  return $ pictures [grassBg, terrainPic]
    
