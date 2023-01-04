module Debug.Debug 
  ( debugGrid
  , debugPoint
  , debugPointWithCoords
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

-- TODO
debugGrid :: Picture
debugGrid = line [(0, 100), (0, 200)]
  where
    size = 64