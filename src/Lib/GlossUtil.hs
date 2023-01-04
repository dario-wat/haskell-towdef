module Lib.GlossUtil
  ( thickRectangleWire 
  ) where

import Graphics.Gloss (Picture, rectangleWire, pictures)
 
-- Draw a rectangle with a border of given thickness. Complexity is O(n) where
-- n is the thickness so we shouldn't use this with high numbers
thickRectangleWire :: Float -> Float -> Float -> Picture
thickRectangleWire thickness w h = 
  pictures $ map rectangleWithThickness $ filter isValidThickness [minTh..maxTh]
  where 
    floorF = fromIntegral . (floor :: Float -> Int)
    (minTh, maxTh) = (floorF $ -thickness / 2, floorF thickness / 2)
    isValidThickness t = w + t >= 0 && h + t >= 0
    rectangleWithThickness t = rectangleWire (w + t) (h + t)