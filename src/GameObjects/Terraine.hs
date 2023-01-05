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

tileWidth :: Int
tileWidth = 64

tileHeight :: Int
tileHeight = 64

readTerraineImage :: IO DynamicImage
readTerraineImage = readPngOrError "assets/grass_tileset.png"

-- TODO use crop frame here

-- x and y are the coordinates of the tile in the tileset
cropTile :: Int -> Int -> DynamicImage -> Picture
cropTile x y = fromImageRGBA8 . cropFn . convertRGBA8
  where cropFn = crop (x * tileWidth) (y * tileHeight) tileWidth tileHeight

-- x and y are the coordinates of the tile in the tileset
-- w and h are the number of tiles representing width and height
cropTiles :: Int -> Int -> Int -> Int -> DynamicImage -> Picture
cropTiles x y w h = fromImageRGBA8 . cropFn . convertRGBA8
  where cropFn = crop (x * tileWidth) (y * tileHeight) (w * tileWidth) (h * tileHeight)

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
    { horizontalBridge    = cropTiles 7 13 3 2 im
    , verticalBridge      = cropTiles 10 12 3 3 im
    , greenTree1          = cropTile 13 6 im
    , greenTree2          = cropTile 14 6 im
    , greenTree3          = cropTile 13 7 im
    , greenTree4          = cropTile 14 7 im
    , brownTree1          = cropTile 13 9 im
    , brownTree2          = cropTile 14 9 im
    , brownTree3          = cropTile 13 10 im
    , brownTree4          = cropTile 14 10 im
    , rock1               = cropTile 13 12 im
    , rock2               = cropTile 14 12 im
    , rock3               = cropTile 13 13 im
    , rock4               = cropTile 14 13 im
    , bush1               = cropTile 11 9 im
    , bush2               = cropTile 12 9 im
    , rockWallDown        = cropTiles 8 9 3 1 im
    , rockWallUp          = cropTiles 8 10 3 1 im
    , rockWallLeft        = cropTiles 6 10 1 3 im
    , rockWallRight       = cropTiles 7 10 1 3 im
    , rockWallTopLeft     = cropTile 8 11 im
    , rockWallTopRight    = cropTile 9 11 im
    , rockWallBottomLeft  = cropTile 8 12 im
    , rockWallBottomRight = cropTile 9 11 im
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
    { roadCrossing         = cropTiles 1 6 3 3 im
    , roadTopLeft          = cropTiles 11 1 2 2 im
    , roadTopRight         = cropTiles 13 1 2 2 im
    , roadBottomLeft       = cropTiles 11 3 2 2 im
    , roadBottomRight      = cropTiles 13 3 2 2 im
    , roadStrip            = cropTiles 9 1 1 3 im
    , roadTopLeftSharp     = cropTiles 5 1 2 2 im
    , roadTopRightSharp    = cropTiles 6 1 2 2 im
    , roadBottomLeftSharp  = cropTiles 5 2 2 2 im
    , roadBottomRightSharp = cropTiles 6 2 2 2 im
    , grass                = cropTile 2 1 im
    }