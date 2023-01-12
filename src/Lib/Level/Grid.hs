module Lib.Level.Grid 
  ( Grid(..)
  , gridCenterOf
  , gridCols
  , gridRows
  , emptyGrid

  , debugGrid
  , gridArrayStr
  , gridArraysStr
  ) where

import Lib.Window (windowWidth, windowHeight, x', y')
import Data.Array (Array, listArray, elems)
import Data.List.HT (sliceHorizontal)
import Data.List (intercalate)
import qualified Graphics.Gloss as G

newtype Grid = Grid
  { unGrid :: Array (Int, Int) Char
  }
  deriving (Show, Eq)

cellSize :: Float
cellSize = 64   -- same as terraine tile size

gridSize :: (Int, Int)
gridSize = (21, 11) -- fits for 1440x810, minus 1 extra cell for the border

gridCols :: Int
gridCols = fst gridSize

gridRows :: Int
gridRows = snd gridSize

gridCenterOf :: (Int, Int) -> (Int, Int) -> (Float, Float)
gridCenterOf (x, y) (w, h) = (centerX, centerY)
  where
    centerX = gridX x + fromIntegral w * cellSize / 2
    centerY = gridY y + fromIntegral h * cellSize / 2

emptyGrid :: Grid
emptyGrid = Grid $ listArray ((0, 0), (gridCols - 1, gridRows - 1)) $ repeat '.'


-- Following are private functions used for debugging or as helpers

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

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

debugGrid :: G.Picture
debugGrid = G.pictures [vLines, hLines, vNumbers, hNumbers]
  where
    (endX, endY) = (gridStartX + gridWidth, gridStartY + gridHeight)
    gLines = G.pictures . map G.line
    vLines = gLines [[(gridX x, gridStartY), (gridX x, endY)] | x <- [0..gridCols]]
    hLines = gLines [[(gridStartX, gridY y), (endX, gridY y)] | y <- [0..gridRows]]
    text = G.scale 0.1 0.1 . G.text . show
    vNumbers = G.pictures [G.translate (gridX x + 20) (endY + 16) $ text x | x <- [0..gridCols-1]]
    hNumbers = G.pictures [G.translate (gridStartX - 20) (gridY y + 20) $ text y | y <- [0..gridRows-1]]

gridArrayStr :: Grid -> String
gridArrayStr = intercalate "\n" . sliceHorizontal gridRows . elems . unGrid

gridArraysStr :: [Grid] -> String
gridArraysStr = intercalate "\n\n" . map gridArrayStr