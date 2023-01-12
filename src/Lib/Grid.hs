{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Lib.Grid where

import Lib.Window (windowWidth, windowHeight, x', y')
import Data.Array (Array, listArray, elems)
import Data.List.HT (sliceHorizontal)
import Data.List (intercalate)

type GridArray = Array (Int, Int) Char

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

-- | x coordinate of the leftmost cell
gridStartX :: Float
gridStartX = x' $ fst gridPadding / 2

-- | y coordinate of the topmost cell
gridStartY :: Float
gridStartY = y' (snd gridPadding / 2) + gridYOffset
  where gridYOffset = -20

-- | Bottom left x coordinate of a grid cell
gridX :: Int -> Float
gridX x = gridStartX + fromIntegral x * cellSize

-- | Bottom left y coordinate of a grid cell
gridY :: Int -> Float
gridY y = gridStartY + fromIntegral y * cellSize

-- | Center x coordinate of a grid cell
gridCX :: Int -> Float
gridCX x = gridX x + cellSize / 2

-- | Center y coordinate of a grid cell
gridCY :: Int -> Float
gridCY y = gridY y + cellSize / 2

gridCenterOf :: (Int, Int) -> (Int, Int) -> (Float, Float)
gridCenterOf (x, y) (w, h) = (centerX, centerY)
  where
    centerX = gridX x + fromIntegral w * cellSize / 2
    centerY = gridY y + fromIntegral h * cellSize / 2

emtpyGrid :: GridArray
emtpyGrid = listArray ((0, 0), (gridCols - 1, gridRows - 1)) $ repeat '.'