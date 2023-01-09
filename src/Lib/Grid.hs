{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Lib.Grid where

import Lib.Window (windowWidth, windowHeight, x', y')

cellSize :: Float
cellSize = 64   -- same as terraine tile size

gridSize :: (Int, Int)
gridSize = (21, 11) -- fits for 1440x810, minus 1 extra cell for the border

gridCols :: Int
gridCols = fst gridSize

gridRows :: Int
gridRows = snd gridSize

gridDimensions :: (Float, Float)
gridDimensions = (fromIntegral gridCols * cellSize, fromIntegral gridRows * cellSize)

gridWidth :: Float
gridWidth = fst gridDimensions

gridHeight :: Float
gridHeight = snd gridDimensions

gridPadding :: (Float, Float)
gridPadding = (windowWidth - gridWidth, windowHeight - gridHeight)

gridYOffset :: Float
gridYOffset = -20

-- | x coordinate of the leftmost cell
gridX :: Float
gridX = x' $ fst gridPadding / 2

-- | y coordinate of the topmost cell
gridY :: Float
gridY = y' (snd gridPadding / 2) + gridYOffset

gridRow :: Int -> Float
gridRow row = gridY + fromIntegral row * cellSize

gridCol :: Int -> Float
gridCol col = gridX + fromIntegral col * cellSize