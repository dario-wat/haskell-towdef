{-# LANGUAGE NamedFieldPuns #-}

module GameObjects.Terrain
  ( Tile(..)
  , TerrainObjects(..)
  , TerrainTiles(..)
  , WaterTiles(..)
  , terrainObjects
  , terrainTiles
  , mkSpriteFromTile
  , waterTiles
  ) where

import Codec.Picture (DynamicImage, convertRGBA8)
import Codec.Picture.Extra (crop)
import ThirdParty.GraphicsGlossJuicy (fromImageRGBA8)
import ThirdParty.GraphicsGlossGame (animation)
import Const (spriteWidth, spriteHeight)
import Lib.Animation (mkInfAnimation)
import qualified GameObjects.Sprite as S
import Lib.Image (readPngOrError)
import Lib.Level.Grid (gridCenterOf)
import Lib.Spritesheet (animFrames)

-- TODO
-- 1. Will likely need to do the usual draw + update thing for water

data Tile = Tile 
  { visual :: !S.Visual
  , w      :: !Int
  , h      :: !Int
  }

type TileOnGrid = (Int, Int, Tile)

tileWidth :: Int
tileWidth = spriteWidth

tileHeight :: Int
tileHeight = spriteHeight

readTerrainImage :: IO DynamicImage
readTerrainImage = readPngOrError "assets/grass_tileset.png"

readWaterTilesetImage :: IO DynamicImage
readWaterTilesetImage = readPngOrError "assets/water_tileset.png"

-- r and c are the coordinates of the tile in the tileset
cropTile :: Int -> Int -> DynamicImage -> Tile
cropTile r c = cropTiles r c 1 1

-- r and c are the coordinates of the tile in the tileset
-- w and h are the number of tiles representing width and height
cropTiles :: Int -> Int -> Int -> Int -> DynamicImage -> Tile
cropTiles r c w h img = Tile (S.Pic $ fromImageRGBA8 $ cropFn $ convertRGBA8 img) w h
  where cropFn = crop (c * tileWidth) (r * tileHeight) (w * tileWidth) (h * tileHeight)

-- | Creates a sprite from a tile.
-- Given row and column are the bottom left tile of multi cell tiles. E.g. if a
-- tile is 3x3, the given row and column will be the coordinates of the grid
-- where the bottom left cell will be drawn.
mkSpriteFromTile :: TileOnGrid -> S.Sprite
mkSpriteFromTile (x, y, Tile{visual=S.Pic pic,   w, h}) = S.mkStaticSprite xc yc pic
  where (xc, yc) = gridCenterOf (x, y) (w, h)
mkSpriteFromTile (x, y, Tile{visual=S.Anim anim, w, h}) = S.mkAnimatedSprite xc yc anim
  where (xc, yc) = gridCenterOf (x, y) (w, h)

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

data WaterTiles = WaterTiles
  { left               :: !Tile
  , right              :: !Tile
  , top                :: !Tile
  , bottom             :: !Tile
  , topLeftConcave     :: !Tile
  , topRightConcave    :: !Tile
  , bottomLeftConcave  :: !Tile
  , bottomRightConcave :: !Tile
  , topLeftConvex      :: !Tile
  , topRightConvex     :: !Tile
  , bottomLeftConvex   :: !Tile
  , bottomRightConvex  :: !Tile
  , fullCalm           :: !Tile
  , fullWave1          :: !Tile
  , fullWave2          :: !Tile
  , fullWave3          :: !Tile
  , fullBubbles1       :: !Tile
  , fullBubbles2       :: !Tile
  , fullFish           :: !Tile
  }

waterTiles :: IO WaterTiles
waterTiles = do
  img <- readWaterTilesetImage
  let 
    waterTile (r, c) = Tile
      { visual = S.Anim 
          $ mkInfAnimation 
          $ animation (animFrames size (r, c, frameCount, tileStep) img) animDt
      , w
      , h
      }
    size = (64, 64)
    frameCount = 10
    tileStep = 7
    animDt = 0.1
    (w, h) = (1, 1)
  return $ WaterTiles
    { left               = waterTile (3, 0)
    , right              = waterTile (3, 6)
    , top                = waterTile (0, 3)
    , bottom             = waterTile (6, 3)
    , topLeftConcave     = waterTile (4, 4)
    , topRightConcave    = waterTile (4, 2)
    , bottomLeftConcave  = waterTile (2, 4)
    , bottomRightConcave = waterTile (2, 2)
    , topLeftConvex      = waterTile (0, 2)
    , topRightConvex     = waterTile (0, 4)
    , bottomLeftConvex   = waterTile (6, 2)
    , bottomRightConvex  = waterTile (6, 4)
    , fullCalm           = waterTile (5, 3)
    , fullWave1          = waterTile (1, 3)
    , fullWave2          = waterTile (3, 3)
    , fullWave3          = waterTile (1, 3)
    , fullBubbles1       = waterTile (3, 4)
    , fullBubbles2       = waterTile (3, 5)
    , fullFish           = waterTile (4, 3)
    }