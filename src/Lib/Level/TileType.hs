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
  | GreenTree
  | BrownTree
  | Rock
  | Bush
  | Water
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
tileTypeChar GreenTree      = 'G'
tileTypeChar BrownTree      = 'B'
tileTypeChar Rock           = 'R'
tileTypeChar Bush           = 'b'
tileTypeChar Water          = 'W'