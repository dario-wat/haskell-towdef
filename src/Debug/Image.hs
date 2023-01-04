module Debug.Image
  ( debugPoint
  , debugPointWithCoords
  , debugBoundingBox
  ) where

import Graphics.Gloss

debugPoint :: Float -> Float -> Picture
debugPoint x y = translate x y $ color red $ circleSolid 5

coordinate :: Float -> Float -> Picture
coordinate x y = translate (x + xOff) y $ textScale $ text coordText
  where 
    xOff = 10
    coordText = show (round x :: Int, round y :: Int)
    textScale = scale 0.1 0.1

debugPointWithCoords :: Float -> Float -> Picture
debugPointWithCoords x y = pictures [debugPoint x y, coordinate x y]

bitmapSizeF :: BitmapData -> (Float, Float)
bitmapSizeF bData = (fromIntegral w, fromIntegral h)
  where (w, h) = bitmapSize bData

debugBoundingBox :: Picture -> ((Float, Float), (Float, Float))
debugBoundingBox (Bitmap bData) = ((0, 0), bitmapSizeF bData)
debugBoundingBox _              = error "boundingBox: not a bitmap"