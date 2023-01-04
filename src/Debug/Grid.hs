module Debug.Grid 
  ( debugGrid
  ) where

import Lib.Grid (cellSize)
import Lib.Window (windowBottomLeft, windowTopRight)
import Graphics.Gloss (Picture, pictures, line)

-- TODO right now it draws the entire window, but it should be bounded
-- by a rectangle. It should use grid size and cell size to determine
debugGrid :: Picture
debugGrid = pictures $ map line $ verticalLines ++ horizontalLines
  where
    (startX, startY) = windowBottomLeft
    (endX, endY) = windowTopRight
    cellSizeF = fromIntegral cellSize
    verticalLines   = map (\x -> [(x, startY), (x, endY)]) [startX, startX + cellSizeF .. endX]
    horizontalLines = map (\y -> [(startX, y), (endX, y)]) [startY, startY + cellSizeF .. endY]