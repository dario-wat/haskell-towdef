module Debug.Debug 
  ( debugGrid
  , debugPoint
  , debugPointWithCoords
  ) where

import Config.Const (cellSize)
import Config.Window (windowBottomLeft, windowTopRight)
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

debugGrid :: Picture
debugGrid = pictures $ map line $ verticalLines ++ horizontalLines
  where
    (startX, startY) = windowBottomLeft
    (endX, endY) = windowTopRight
    cellSizeF = fromIntegral cellSize
    verticalLines   = map (\x -> [(x, startY), (x, endY)]) [startX, startX + cellSizeF .. endX]
    horizontalLines = map (\y -> [(startX, y), (endX, y)]) [startY, startY + cellSizeF .. endY]