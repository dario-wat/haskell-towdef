{-# LANGUAGE TupleSections #-}

module Lib.Level.MapGenerator 
  ( picturizeGrid
  , generateGrid
  ) where

import System.Random (randomRIO)
import Data.Array (assocs, (//))
import qualified Graphics.Gloss as G
import Lib.Level.Grid (Grid(..), emptyGrid)
import Lib.Level.Path (genRandomPath, addPathToGrid)
import qualified Lib.Level.TileType as TT
import Lib.Util (chooseRandom)
import qualified GameObjects.Terrain as T
import Graphics.Gloss (pictures)
import GameObjects.Terrain (greenTrees, brownTrees)

greenTreeRange :: (Int, Int)
greenTreeRange = (3, 6)

brownTreeRange :: (Int, Int)
brownTreeRange = (1, 6)

generateGrid :: IO Grid
generateGrid = addPathToGrid emptyGrid <$> genRandomPath

randomGreenTree :: IO T.Tile
randomGreenTree = head <$> (chooseRandom 1 =<< greenTrees)

randomBrownTree :: IO T.Tile
randomBrownTree = head <$> (chooseRandom 1 =<< brownTrees)

objectsToAdd :: IO [(Int, TT.TileType)]
objectsToAdd = do
  grTreeCount <- randomRIO greenTreeRange
  brTreeCount <- randomRIO brownTreeRange
  return 
    [ (grTreeCount, TT.GreenTree)
    , (brTreeCount, TT.BrownTree)
    ]

addObjectsToGrid :: Grid -> IO Grid
addObjectsToGrid grid = do
  let 
    emptyTiles = filter ((==TT.Empty) . snd) $ assocs $ unGrid grid
    
    addObjects :: [(Int, TT.TileType)] -> [(Int, Int)] -> [((Int, Int), TT.TileType)]
    addObjects ((n, tType):objs) points = zip (take n points) (repeat tType) ++ addObjects objs (drop n points) 
    addObjects [] _ = []
  objs <- objectsToAdd
  let totalCount = sum $ map fst objs
  shuffled <- chooseRandom totalCount emptyTiles
  return $ Grid $ unGrid grid // addObjects objs (map fst shuffled)

drawTerraineWithMap :: (TT.TileType -> T.Tile) -> Grid -> G.Picture
drawTerraineWithMap tf = T.drawTerrain . T.Terrain . map tTypeF . assocs . unGrid
  where tTypeF ((x, y), tType) = (x, y, tf tType)

grassBackground :: IO G.Picture
grassBackground = do
  tTil <- T.terrainTiles
  return $ drawTerraineWithMap (const $ T.grass tTil) emptyGrid

picturizeGrid :: Grid -> IO G.Picture
picturizeGrid grid = do
  tTil <- T.terrainTiles
  tObj <- T.terrainObjects
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
    tileMap TT.GreenTree      = T.greenTree1 tObj   -- TODO
    tileMap TT.BrownTree      = T.brownTree1 tObj   -- TODO

  grassBg <- grassBackground
  newGrid <- addObjectsToGrid grid
  return $ pictures [grassBg, drawTerraineWithMap tileMap newGrid]
    
