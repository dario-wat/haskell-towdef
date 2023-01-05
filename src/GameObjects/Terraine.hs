module GameObjects.Terraine
  ( cropTile
  , cropTiles
  , readTerraineImage
  , terraineObjects
  , terraineTiles
  , TerraineObjects(..)
  , TerraineTiles(..)
  ) where

import ThirdParty.GraphicsGlossJuicy (fromImageRGBA8)
import Lib.Image (readPngOrError)
import Codec.Picture (DynamicImage, convertRGBA8)
import Codec.Picture.Extra (crop)
import Graphics.Gloss (Picture)
import Const (spriteWidth, spriteHeight)

tileWidth :: Int
tileWidth = spriteWidth

tileHeight :: Int
tileHeight = spriteHeight

readTerraineImage :: IO DynamicImage
readTerraineImage = readPngOrError "assets/grass_tileset.png"

-- r and c are the coordinates of the tile in the tileset
cropTile :: Int -> Int -> DynamicImage -> Picture
cropTile r c = cropTiles r c 1 1

-- r and c are the coordinates of the tile in the tileset
-- w and h are the number of tiles representing width and height
cropTiles :: Int -> Int -> Int -> Int -> DynamicImage -> Picture
cropTiles r c w h = fromImageRGBA8 . cropFn . convertRGBA8
  where cropFn = crop (c * tileWidth) (r * tileHeight) (w * tileWidth) (h * tileHeight)

data TerraineObjects = TerraineObjects
  { horizontalBridge :: Picture
  , verticalBridge :: Picture
  , greenTree1 :: Picture
  , greenTree2 :: Picture
  , greenTree3 :: Picture
  , greenTree4 :: Picture
  , brownTree1 :: Picture
  , brownTree2 :: Picture
  , brownTree3 :: Picture
  , brownTree4 :: Picture
  , rock1 :: Picture
  , rock2 :: Picture
  , rock3 :: Picture
  , rock4 :: Picture
  , bush1 :: Picture
  , bush2 :: Picture
  , rockWallDown :: Picture
  , rockWallUp :: Picture
  , rockWallLeft :: Picture
  , rockWallRight :: Picture
  , rockWallTopLeft :: Picture
  , rockWallTopRight :: Picture
  , rockWallBottomLeft :: Picture
  , rockWallBottomRight :: Picture
  }

terraineObjects :: IO TerraineObjects
terraineObjects = do
  im <- readTerraineImage
  return $ TerraineObjects
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

data TerraineTiles = TerraineTiles
  { roadCrossing :: Picture
  , roadTopLeft :: Picture
  , roadTopRight :: Picture
  , roadBottomLeft :: Picture
  , roadBottomRight :: Picture
  , roadStrip :: Picture
  , roadTopLeftSharp :: Picture
  , roadTopRightSharp :: Picture
  , roadBottomLeftSharp :: Picture
  , roadBottomRightSharp :: Picture
  , grass :: Picture
  }

terraineTiles :: IO TerraineTiles
terraineTiles = do
  im <- readTerraineImage
  return $ TerraineTiles
    { roadCrossing         = cropTiles 6  1 3 3 im
    , roadTopLeft          = cropTiles 1 11 2 2 im
    , roadTopRight         = cropTiles 1 13 2 2 im
    , roadBottomLeft       = cropTiles 3 11 2 2 im
    , roadBottomRight      = cropTiles 3 13 2 2 im
    , roadStrip            = cropTiles 1  9 1 3 im
    , roadTopLeftSharp     = cropTiles 1  5 2 2 im
    , roadTopRightSharp    = cropTiles 1  6 2 2 im
    , roadBottomLeftSharp  = cropTiles 2  5 2 2 im
    , roadBottomRightSharp = cropTiles 2  6 2 2 im
    , grass                = cropTile  1  2 im
    }