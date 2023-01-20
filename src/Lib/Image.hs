module Lib.Image
  ( readPngOrError
  , thickRectangleWire
  , boundingBox
  ) where

import Data.Either (fromRight)
import Codec.Picture (DynamicImage, readPng)
import qualified Graphics.Gloss as G
import Lib.Util (both)

readPngOrError :: FilePath -> IO DynamicImage
readPngOrError filepath = fromRight (error errorText) <$> readPng filepath
  where errorText = "no image: " <> filepath

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

-- NOTE works only on bitmaps
boundingBox :: G.Picture -> G.Picture
boundingBox (G.Bitmap bData) = G.color G.red $ thickRectangleWire 2 w h
                                 where (w, h) = both fromIntegral $ G.bitmapSize bData
boundingBox _                = error "boundingBox: not a bitmap"