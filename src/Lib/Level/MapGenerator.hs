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

-- TODO
-- Make corners rounded
-- Make enemies follow the path
-- Water generate and draw
-- Use sprites for tiles

-- TODO what is this?
generateGrid :: IO Grid
generateGrid = addPathToGrid emptyGrid <$> genRandomPath

greenTreeRange :: (Int, Int)
greenTreeRange = (8, 12)

brownTreeRange :: (Int, Int)
brownTreeRange = (7, 10)

rockRange :: (Int, Int)
rockRange = (6, 8)

bushRange :: (Int, Int)
bushRange = (4, 6)

objectsToAdd :: IO [(Int, TT.TileType)]
objectsToAdd = do
  grTreeCount <- randomRIO greenTreeRange
  brTreeCount <- randomRIO brownTreeRange
  rockCount   <- randomRIO rockRange
  bushCount   <- randomRIO bushRange
  return 
    [ (grTreeCount, TT.GreenTree)
    , (brTreeCount, TT.BrownTree)
    , (rockCount,   TT.Rock)
    , (bushCount,   TT.Bush)
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

drawTerraineWithMap :: (TT.TileType -> IO T.Tile) -> Grid -> IO G.Picture
drawTerraineWithMap tf grid = T.drawTerrain . T.Terrain <$> mapM tTypeF (assocs $ unGrid grid)
  where tTypeF ((x, y), tType) = (x, y,) <$> tf tType

picturizeGrid :: Grid -> IO G.Picture
picturizeGrid grid = do
  tTil <- T.terrainTiles
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
    tileMapM TT.GreenTree      = randomTile =<< T.greenTrees
    tileMapM TT.BrownTree      = randomTile =<< T.brownTrees
    tileMapM TT.Rock           = randomTile =<< T.rocks
    tileMapM TT.Bush           = randomTile =<< T.bushes

  grassBg <- grassBackground
  newGrid <- addObjectsToGrid grid
  terrainPic <- drawTerraineWithMap tileMapM newGrid
  return $ pictures [grassBg, terrainPic]
    
