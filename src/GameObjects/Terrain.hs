module GameObjects.Terrain
  ( Tile(..)
  , Terrain(..)
  , TerrainObjects(..)
  , TerrainTiles(..)
  , WaterAnimations(..)
  , terrainObjects
  , terrainTiles
  , drawTerrain
  , greenTrees
  , brownTrees
  , rocks
  , bushes
  , waterAnimations
  ) where

import Data.Tuple.HT (uncurry3)
import Codec.Picture (DynamicImage, convertRGBA8)
import Codec.Picture.Extra (crop)
import Graphics.Gloss (Picture, pictures, translate)
import ThirdParty.GraphicsGlossJuicy (fromImageRGBA8)
import ThirdParty.GraphicsGlossGame (animation)
import Const (spriteWidth, spriteHeight)
import Lib.Animation (MkAnimation)
import Lib.Image (readPngOrError)
import Lib.Level.Grid (gridCenterOf)
import Lib.Spritesheet (animFrames)

-- TODO
-- 1. Will likely need to do the usual draw + update thing for water

data Tile = Tile 
  { picture :: !Picture
  , width   :: !Int
  , height  :: !Int
  } 
  deriving (Show, Eq)

newtype Terrain = Terrain 
  { unTerrain :: [(Int, Int, Tile)] 
  }
  deriving (Show, Eq)

tileWidth :: Int
tileWidth = spriteWidth

tileHeight :: Int
tileHeight = spriteHeight

readTerrainImage :: IO DynamicImage
readTerrainImage = readPngOrError "assets/grass_tileset.png"

-- r and c are the coordinates of the tile in the tileset
cropTile :: Int -> Int -> DynamicImage -> Tile
cropTile r c = cropTiles r c 1 1

-- r and c are the coordinates of the tile in the tileset
-- w and h are the number of tiles representing width and height
cropTiles :: Int -> Int -> Int -> Int -> DynamicImage -> Tile
cropTiles r c w h img = Tile (fromImageRGBA8 $ cropFn $ convertRGBA8 img) w h
  where cropFn = crop (c * tileWidth) (r * tileHeight) (w * tileWidth) (h * tileHeight)

-- | Draws a tile onto the grid
-- Given row and column are the bottom left tile of multi cell tiles. E.g. if a
-- tile is 3x3, the given row and column will be the coordinates of the grid
-- where the bottom left cell will be drawn.
drawTile :: Int -> Int -> Tile -> Picture
drawTile x y (Tile pic w h) = translate xc yc pic
  where (xc, yc) = gridCenterOf (x, y) (w, h)

drawTerrain :: Terrain -> Picture
drawTerrain = pictures . map (uncurry3 drawTile) . unTerrain

data TerrainObjects = TerrainObjects
  { horizontalBridge    :: !Tile
  , verticalBridge      :: !Tile
  , greenTree1          :: !Tile
  , greenTree2          :: !Tile
  , greenTree3          :: !Tile
  , greenTree4          :: !Tile
  , brownTree1          :: !Tile
  , brownTree2          :: !Tile
  , brownTree3          :: !Tile
  , brownTree4          :: !Tile
  , rock1               :: !Tile
  , rock2               :: !Tile
  , rock3               :: !Tile
  , rock4               :: !Tile
  , bush1               :: !Tile
  , bush2               :: !Tile
  , rockWallDown        :: !Tile
  , rockWallUp          :: !Tile
  , rockWallLeft        :: !Tile
  , rockWallRight       :: !Tile
  , rockWallTopLeft     :: !Tile
  , rockWallTopRight    :: !Tile
  , rockWallBottomLeft  :: !Tile
  , rockWallBottomRight :: !Tile
  }

terrainObjects :: IO TerrainObjects
terrainObjects = do
  im <- readTerrainImage
  return $ TerrainObjects
    { horizontalBridge    = cropTiles 13  7 3 2 im
    , verticalBridge      = cropTiles 12 10 3 3 im
    , greenTree1          = cropTile   6 13 im
    , greenTree2          = cropTile   6 14 im
    , greenTree3          = cropTile   7 13 im
    , greenTree4          = cropTile   7 14 im
    , brownTree1          = cropTile   9 13 im
    , brownTree2          = cropTile   9 14 im
    , brownTree3          = cropTile  10 13 im
    , brownTree4          = cropTile  10 14 im
    , rock1               = cropTile  12 13 im
    , rock2               = cropTile  12 14 im
    , rock3               = cropTile  13 13 im
    , rock4               = cropTile  13 14 im
    , bush1               = cropTile   9 11 im
    , bush2               = cropTile   9 12 im
    , rockWallDown        = cropTiles  9  8  3 1 im
    , rockWallUp          = cropTiles 10  8  3 1 im
    , rockWallLeft        = cropTiles 10  6  1 3 im
    , rockWallRight       = cropTiles 10  7  1 3 im
    , rockWallTopLeft     = cropTile  11  8 im
    , rockWallTopRight    = cropTile  11  9 im
    , rockWallBottomLeft  = cropTile  12  8 im
    , rockWallBottomRight = cropTile  11  9 im
    }

greenTrees :: IO [Tile]
greenTrees = do
  tObj <- terrainObjects
  return [greenTree1 tObj, greenTree2 tObj, greenTree3 tObj, greenTree4 tObj]

brownTrees :: IO [Tile]
brownTrees = do
  tObj <- terrainObjects
  return [brownTree1 tObj, brownTree2 tObj, brownTree3 tObj, brownTree4 tObj]

rocks :: IO [Tile]
rocks = do
  tObj <- terrainObjects
  return [rock1 tObj, rock2 tObj, rock3 tObj, rock4 tObj]

bushes :: IO [Tile]
bushes = do
  tObj <- terrainObjects
  return [bush1 tObj, bush2 tObj]

data TerrainTiles = TerrainTiles
  { roadCrossing         :: !Tile
  , roadTopLeft          :: !Tile
  , roadTopRight         :: !Tile
  , roadBottomLeft       :: !Tile
  , roadBottomRight      :: !Tile
  , roadTopLeftSharp     :: !Tile
  , roadTopRightSharp    :: !Tile
  , roadBottomLeftSharp  :: !Tile
  , roadBottomRightSharp :: !Tile
  , grass                :: !Tile
  , roadVertical         :: !Tile
  , roadHorizontal       :: !Tile
  }

terrainTiles :: IO TerrainTiles
terrainTiles = do
  im <- readTerrainImage
  return $ TerrainTiles
    { roadCrossing         = cropTile  7  2 im
    , roadTopLeft          = cropTiles 1 11 2 2 im
    , roadTopRight         = cropTiles 1 13 2 2 im
    , roadBottomLeft       = cropTiles 3 11 2 2 im
    , roadBottomRight      = cropTiles 3 13 2 2 im
    , roadTopLeftSharp     = cropTile  1  5 im
    , roadTopRightSharp    = cropTile  1  7 im
    , roadBottomLeftSharp  = cropTile  3  5 im
    , roadBottomRightSharp = cropTile  3  7 im
    , grass                = cropTile  1  2 im
    , roadVertical         = cropTile  2  5 im
    , roadHorizontal       = cropTile  1  6 im
    }

data WaterAnimations = WaterAnimations
  { left               :: !MkAnimation
  , right              :: !MkAnimation
  , top                :: !MkAnimation
  , bottom             :: !MkAnimation
  , topLeftConcave     :: !MkAnimation
  , topRightConcave    :: !MkAnimation
  , bottomLeftConcave  :: !MkAnimation
  , bottomRightConcave :: !MkAnimation
  , topLeftConvex      :: !MkAnimation
  , topRightConvex     :: !MkAnimation
  , bottomLeftConvex   :: !MkAnimation
  , bottomRightConvex  :: !MkAnimation
  , fullCalm           :: !MkAnimation
  , fullWave1          :: !MkAnimation
  , fullWave2          :: !MkAnimation
  , fullWave3          :: !MkAnimation
  , fullBubbles1       :: !MkAnimation
  , fullBubbles2       :: !MkAnimation
  , fullFish           :: !MkAnimation
  }

waterAnimations :: IO WaterAnimations
waterAnimations = do
  img <- readPngOrError "assets/water_tileset.png"
  let 
    waterAnimation (r, c) = animation (animFrames size (r, c, 10, 7) img) 0.1
    size = (64, 64)
  return $ WaterAnimations
    { left               = waterAnimation (3, 0)
    , right              = waterAnimation (3, 6)
    , top                = waterAnimation (0, 3)
    , bottom             = waterAnimation (6, 3)
    , topLeftConcave     = waterAnimation (4, 4)
    , topRightConcave    = waterAnimation (4, 2)
    , bottomLeftConcave  = waterAnimation (2, 4)
    , bottomRightConcave = waterAnimation (2, 2)
    , topLeftConvex      = waterAnimation (0, 2)
    , topRightConvex     = waterAnimation (0, 4)
    , bottomLeftConvex   = waterAnimation (6, 2)
    , bottomRightConvex  = waterAnimation (6, 4)
    , fullCalm           = waterAnimation (5, 3)
    , fullWave1          = waterAnimation (1, 3)
    , fullWave2          = waterAnimation (3, 3)
    , fullWave3          = waterAnimation (1, 3)
    , fullBubbles1       = waterAnimation (3, 4)
    , fullBubbles2       = waterAnimation (3, 5)
    , fullFish           = waterAnimation (4, 3)
    }