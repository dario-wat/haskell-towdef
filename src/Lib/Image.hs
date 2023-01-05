module Lib.Image
  ( readPngOrError
  , cropFrame
  , dynWidth
  , dynHeight
  ) where

import Codec.Picture (DynamicImage, readPng, dynamicMap, Image (imageWidth, imageHeight), convertRGBA8)
import Data.Either (fromRight)
import Graphics.Gloss (Picture)
import ThirdParty.GraphicsGlossJuicy (fromImageRGBA8)
import Codec.Picture.Extra (crop)

readPngOrError :: FilePath -> IO DynamicImage
readPngOrError filepath = fromRight (error errorText) <$> readPng filepath
  where errorText = "no image: " <> filepath

-- r and c are row and column of the frame in the spritesheet or tileset
cropFrame :: Int -> Int -> Int -> Int -> DynamicImage -> Picture
cropFrame r c w h = fromImageRGBA8 . cropFn . convertRGBA8
  where cropFn = crop (c * w) (r * h) w h

dynWidth :: DynamicImage -> Int
dynWidth = dynamicMap imageWidth

dynHeight :: DynamicImage -> Int
dynHeight = dynamicMap imageHeight