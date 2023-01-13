{-# LANGUAGE TupleSections #-}

module Lib.Level.MapGenerator 
  ( picturizeGrid
  , generateGrid
  ) where

import Control.Monad (forM)
import System.Random (randomRIO)
import Data.Array (assocs, (//), (!))
import qualified Graphics.Gloss as G
import Lib.Level.Grid (Grid(..), emptyGrid)
import Lib.Level.Path (genRandomPath, genRandomPoint, addPathToGrid)
import qualified Lib.Level.TileType as TT
import Lib.Util (inRangeF)
import qualified GameObjects.Terrain as T
import Graphics.Gloss (pictures)

greenTreeProb :: Float
greenTreeProb = 0.05

brownTreeProb :: Float
brownTreeProb = 0.0

generateGrid :: IO Grid
generateGrid = addPathToGrid emptyGrid <$> genRandomPath

randomGreenTree :: IO T.Tile
randomGreenTree = do
  tObj <- T.terrainObjects
  i <- randomRIO (0 :: Int, 3)
  return $ case i of
    0 -> T.greenTree1 tObj
    1 -> T.greenTree2 tObj
    2 -> T.greenTree3 tObj
    3 -> T.greenTree4 tObj

randomBrownTree :: IO T.Tile
randomBrownTree = do
  tObj <- T.terrainObjects
  i <- randomRIO (0 :: Int, 3)
  return $ case i of
    0 -> T.brownTree1 tObj
    1 -> T.brownTree2 tObj
    2 -> T.brownTree3 tObj
    3 -> T.brownTree4 tObj

randomObject :: IO TT.TileType
randomObject = chooseObj <$> randomRIO (0 :: Float, 1) 
  where
    chooseObj :: Float -> TT.TileType
    chooseObj p
      | inRangeF (0, greenTreeProb) p                             = TT.GreenTree
      | inRangeF (greenTreeProb, greenTreeProb + brownTreeProb) p = TT.BrownTree
      | otherwise                                                 = TT.Grass

addTreesToGrid :: Grid -> IO Grid
addTreesToGrid grid = do
  let el = filter ((==TT.Empty) . snd) $ assocs $ unGrid grid
  let packObj (p, _) = (p,) <$>  randomObject
        -- obj <- 
        -- return (p, obj)
    
    -- randomObj :: IO TT.TileType
    -- randomObj = chooseObj <$> randomRIO (0 :: Float, 1) 
  bs <- 
    forM el packObj
  return $ Grid $ unGrid grid // bs

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
    tileMap TT.GreenTree      = T.greenTree1 tObj
    tileMap TT.BrownTree      = T.brownTree1 tObj

    -- TODO percentages should be correct
    insertObjects :: Grid -> IO Grid
    insertObjects grid = do
      obj <- randomObject
      (x, y) <- genRandomPoint
      if unGrid grid ! (x, y) == TT.Empty
        then return $ Grid $ unGrid grid // [((x, y), obj)]
        else insertObjects grid

  grassBg <- grassBackground
  newGrid <- addTreesToGrid grid
  return $ pictures [grassBg, drawTerraineWithMap tileMap newGrid]
    
