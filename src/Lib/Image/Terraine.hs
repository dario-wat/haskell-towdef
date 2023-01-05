{-# LANGUAGE NamedFieldPuns #-}
module Lib.Image.Terraine
  ( cropTile
  , cropTiles
  , drawTile
  , mkDefaultTile
  , mkTile
  , readTerraineImage
  , terraineObjects
  , Tile(..)
  , TerraineObjects(..)
  ) where

import ThirdParty.GraphicsGlossJuicy (fromImageRGBA8)
import Lib.Image (readPngOrError)

import Codec.Picture (DynamicImage, convertRGBA8)
import Codec.Picture.Extra (crop)
import Graphics.Gloss (Picture, translate)

data Tile = Tile
  { x :: Float
  , y :: Float
  , tile :: Picture
  }

drawTile :: Tile -> Picture
drawTile (Tile x y tile) = translate x y tile

mkTile :: Float -> Float -> Picture -> Tile
mkTile x y tile = Tile {x, y, tile}

mkDefaultTile :: Picture -> Tile
mkDefaultTile = mkTile 0 0

data TerraineObjects = TerraineObjects
  { horizontalBridge :: Picture
  , verticalBridge :: Picture
  }

tileWidth :: Int
tileWidth = 64

tileHeight :: Int
tileHeight = 64

readTerraineImage :: IO DynamicImage
readTerraineImage = readPngOrError "assets/Grass Tileset.png"

-- x and y are the coordinates of the tile in the tileset
cropTile :: Int -> Int -> DynamicImage -> Picture
cropTile x y = fromImageRGBA8 . cropFn . convertRGBA8
  where cropFn = crop (x * tileWidth) (y * tileHeight) tileWidth tileHeight

-- x and y are the coordinates of the tile in the tileset
-- w and h are the number of tiles representing width and height
cropTiles :: Int -> Int -> Int -> Int -> DynamicImage -> Picture
cropTiles x y w h = fromImageRGBA8 . cropFn . convertRGBA8
  where cropFn = crop (x * tileWidth) (y * tileHeight) (w * tileWidth) (h * tileHeight)

terraineObjects :: IO TerraineObjects
terraineObjects = do
  im <- readTerraineImage
  return $ TerraineObjects
    { horizontalBridge = cropTiles 7 13 3 2 im
    , verticalBridge = cropTiles 10 12 3 3 im
    }
