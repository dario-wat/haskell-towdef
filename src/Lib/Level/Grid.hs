{-# LANGUAGE TupleSections #-}

module Lib.Level.Grid 
  ( Grid(..)
  , Point
  , gridCenterOf
  , gridCellOf
  , gridCols
  , gridRows
  , emptyGrid

  , genRandomPoints
  , genRandomEdgePoint
  , pointQuadrant

  , debugGrid
  , gridArrayStr
  , gridArraysStr
  ) where

import System.Random (randomRIO)
import Data.Array (Array, listArray, elems)
import Data.List.HT (sliceHorizontal)
import Data.List (intercalate)
import qualified Graphics.Gloss as G
import Lib.Window (windowWidth, windowHeight, x', y')
import Lib.Level.TileType (TileType(Empty), tileTypeChar)

type Point = (Int, Int)

newtype Grid = Grid
  { unGrid :: Array (Int, Int) TileType
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

gridCellOf :: (Float, Float) -> (Int, Int)
gridCellOf (x, y) = (cellX, cellY)
  where
    cellX = floor $ (x - gridStartX) / cellSize
    cellY = floor $ (y - gridStartY) / cellSize

emptyGrid :: Grid
emptyGrid = Grid $ listArray ((0, 0), (gridCols - 1, gridRows - 1)) $ repeat Empty

--------------------
-- PRIVATE --
--------------------

gridDimensions :: (Float, Float)
gridDimensions = (fromIntegral gridCols * cellSize, fromIntegral gridRows * cellSize)

gridWidth :: Float
gridWidth = fst gridDimensions

gridHeight :: Float
gridHeight = snd gridDimensions

gridPadding :: (Float, Float)
gridPadding = (windowWidth - gridWidth, windowHeight - gridHeight)

-- | x coordinate of the left edge
gridStartX :: Float
gridStartX = x' $ fst gridPadding / 2

-- | y coordinate of the top edge
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
-- Point
-------------------------------------------------------------------------------

genRandomPoint :: IO Point
genRandomPoint = do
  x <- randomRIO (0, gridCols - 1)
  y <- randomRIO (0, gridRows - 1)
  return (x, y)

-- | Generates a list of n unique random grid points
genRandomPoints :: Int -> IO [Point]
genRandomPoints n = genMorePoints []
  where
    genMorePoints curr
      | length curr == n = return curr
      | otherwise        = do
        p <- genRandomPoint
        if p `elem` curr then genMorePoints curr else genMorePoints (p : curr)

genRandomEdgePoint :: IO Point
genRandomEdgePoint = do
  edge <- randomRIO (0 :: Int, 3)
  case edge of
    0 -> (,0) <$> randomRIO (0, gridCols - 1)
    1 -> (, gridRows-1) <$> randomRIO (0, gridCols - 1)
    2 -> (0,) <$> randomRIO (0, gridRows - 1)
    3 -> (gridCols-1,) <$> randomRIO (0, gridRows - 1)
    _ -> error "genRandomEdgePoint: impossible"

pointQuadrant :: Point -> Int
pointQuadrant (x, y)
  | x < gridCols `div` 2 && y < gridRows `div` 2 = 1
  | x < gridCols `div` 2 && y >= gridRows `div` 2 = 2
  | x >= gridCols `div` 2 && y < gridRows `div` 2 = 3
  | x >= gridCols `div` 2 && y >= gridRows `div` 2 = 4
  | otherwise = error "pointQuadrant: impossible"

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
gridArrayStr = 
  intercalate "\n" . sliceHorizontal gridRows . map tileTypeChar . elems . unGrid

gridArraysStr :: [Grid] -> String
gridArraysStr = intercalate "\n\n" . map gridArrayStr