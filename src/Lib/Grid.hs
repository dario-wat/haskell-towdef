module Lib.Grid
  ( cellSize
  , gridSize
  , gridCols
  , gridRows
  , gridDimensions
  , gridWidth
  , gridHeight
  , gridPadding
  ) where

import Lib.Window (windowWidth, windowHeight)

cellSize :: Int
cellSize = 64

gridSize :: (Int, Int)
gridSize = (21, 11) -- fits for 1440x810, minus 1 extra cell for the border

gridCols :: Int
gridCols = fst gridSize

gridRows :: Int
gridRows = snd gridSize

gridDimensions :: (Int, Int)
gridDimensions = (gridCols * cellSize, gridRows * cellSize)

gridWidth :: Int
gridWidth = fst gridDimensions

gridHeight :: Int
gridHeight = snd gridDimensions


-- TODO here I would need to calculate the position of the grid. It should include
-- padding from the sides and top and bottom. And we should leave space at the top
-- (or the bottom) for the score and other stuff.

gridPadding :: (Int, Int)
gridPadding = (windowWidth - gridWidth, windowHeight - gridHeight)

