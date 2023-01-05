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
    , verticalBridge   = cropTiles 10 12 3 3 im
    , greenTree1       = cropTile 13 6 im
    , greenTree2       = cropTile 14 6 im
    , greenTree3       = cropTile 13 7 im
    , greenTree4       = cropTile 14 7 im
    , brownTree1       = cropTile 13 9 im
    , brownTree2       = cropTile 14 9 im
    , brownTree3       = cropTile 13 10 im
    , brownTree4       = cropTile 14 10 im
    , rock1            = cropTile 13 12 im
    , rock2            = cropTile 14 12 im
    , rock3            = cropTile 13 13 im
    , rock4            = cropTile 14 13 im
    }
