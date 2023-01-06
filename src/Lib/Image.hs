module Lib.Image
  ( readPngOrError
  , cropFrame
  , cropFrameAndFlip
  , dynWidth
  , dynHeight
  , CropFn
  ) where

import Codec.Picture (DynamicImage, readPng, dynamicMap, Image (imageWidth, imageHeight), convertRGBA8, PixelRGBA8)
import Data.Either (fromRight)
import Graphics.Gloss (Picture)
import ThirdParty.GraphicsGlossJuicy (fromImageRGBA8)
import Codec.Picture.Extra (crop, flipHorizontally)

type CropFn = Int -> Int -> Int -> Int -> DynamicImage -> Picture

readPngOrError :: FilePath -> IO DynamicImage
readPngOrError filepath = fromRight (error errorText) <$> readPng filepath
  where errorText = "no image: " <> filepath

-- r and c are row and column of the frame in the spritesheet or tileset
cropFrameDyn :: Int -> Int -> Int -> Int -> DynamicImage -> Image PixelRGBA8
cropFrameDyn r c w h = crop (c * w) (r * h) w h . convertRGBA8

cropFrame :: Int -> Int -> Int -> Int -> DynamicImage -> Picture
cropFrame r c w h = fromImageRGBA8 . cropFrameDyn r c w h

cropFrameAndFlip :: Int -> Int -> Int -> Int -> DynamicImage -> Picture
cropFrameAndFlip r c w h = fromImageRGBA8 . flipHorizontally . cropFrameDyn r c w h

dynWidth :: DynamicImage -> Int
dynWidth = dynamicMap imageWidth

dynHeight :: DynamicImage -> Int
dynHeight = dynamicMap imageHeight