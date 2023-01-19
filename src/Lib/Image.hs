module Lib.Image
  ( readPngOrError
  , cropFrame
  , cropFrameAndFlip
  , dynWidth
  , dynHeight
  , thickRectangleWire
  , boundingBox
  , CropFn
  ) where

-- TODO WIP

import Codec.Picture (DynamicImage, readPng, dynamicMap, Image (imageWidth, imageHeight), convertRGBA8, PixelRGBA8)
import Data.Either (fromRight)
import qualified Graphics.Gloss as G
import ThirdParty.GraphicsGlossJuicy (fromImageRGBA8)
import Codec.Picture.Extra (crop, flipHorizontally)

type CropFn = Int -> Int -> Int -> Int -> DynamicImage -> G.Picture

readPngOrError :: FilePath -> IO DynamicImage
readPngOrError filepath = fromRight (error errorText) <$> readPng filepath
  where errorText = "no image: " <> filepath

-- r and c are row and column of the frame in the spritesheet or tileset
cropFrameDyn :: Int -> Int -> Int -> Int -> DynamicImage -> Image PixelRGBA8
cropFrameDyn r c w h = crop (c * w) (r * h) w h . convertRGBA8

cropFrame :: Int -> Int -> Int -> Int -> DynamicImage -> G.Picture
cropFrame r c w h = fromImageRGBA8 . cropFrameDyn r c w h

cropFrameAndFlip :: Int -> Int -> Int -> Int -> DynamicImage -> G.Picture
cropFrameAndFlip r c w h = fromImageRGBA8 . flipHorizontally . cropFrameDyn r c w h

dynWidth :: DynamicImage -> Int
dynWidth = dynamicMap imageWidth

dynHeight :: DynamicImage -> Int
dynHeight = dynamicMap imageHeight

-- Draw a rectangle with a border of given thickness. Complexity is O(n) where
-- n is the thickness so we shouldn't use this with high numbers
thickRectangleWire :: Float -> Float -> Float -> G.Picture
thickRectangleWire thickness w h = 
  G.pictures $ map rectangleWithThickness $ filter isValidThickness [minTh..maxTh]
  where 
    floorF = fromIntegral . (floor :: Float -> Int)
    (minTh, maxTh) = (floorF $ -thickness / 2, floorF thickness / 2)
    isValidThickness t = w + t >= 0 && h + t >= 0
    rectangleWithThickness t = G.rectangleWire (w + t) (h + t)

bitmapSizeF :: G.BitmapData -> (Float, Float)
bitmapSizeF bData = (fromIntegral w, fromIntegral h)
  where (w, h) = G.bitmapSize bData

-- NOTE works only on bitmaps
boundingBox :: G.Picture -> G.Picture
boundingBox (G.Bitmap bData) = G.color G.red $ thickRectangleWire 2 w h
  where (w, h) = bitmapSizeF bData
boundingBox _                = error "boundingBox: not a bitmap"