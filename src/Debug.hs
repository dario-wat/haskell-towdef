module Debug 
  ( point
  , pointWithCoords
  ) where

import Graphics.Gloss

point :: Float -> Float -> Picture
point x y = translate x y $ color red $ circleSolid 5

coordinate :: Float -> Float -> Picture
coordinate x y = translate (x + xOff) y $ textScale $ text coordText
  where 
    xOff = 10
    coordText = show (round x :: Int, round y :: Int)
    textScale = scale 0.1 0.1

pointWithCoords :: Float -> Float -> Picture
pointWithCoords x y = pictures [point x y, coordinate x y]