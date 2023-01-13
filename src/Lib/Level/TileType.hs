module Lib.Level.TileType 
  ( TileType(..)
  , tileTypeChar
  ) where

data TileType = 
  Empty
  | RoadHorizontal
  | RoadVertical
  | RoadUpLeft
  | RoadUpRight
  | RoadDownLeft
  | RoadDownRight
  | RoadCrossing
  | Grass
  deriving (Show, Eq)

tileTypeChar :: TileType -> Char
tileTypeChar Empty          = '.'
tileTypeChar RoadHorizontal = '─'
tileTypeChar RoadVertical   = '│'
tileTypeChar RoadUpLeft     = '┐'
tileTypeChar RoadUpRight    = '┌'
tileTypeChar RoadDownLeft   = '┘'
tileTypeChar RoadDownRight  = '└'
tileTypeChar RoadCrossing   = '┼'
tileTypeChar Grass          = '#'

