module Lib.Level.TileType 
  ( TileType(..)
  , tileTypeChar
  , greenTrees
  , brownTrees
  , rocks
  , bushes
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
  | GreenTree1
  | GreenTree2
  | GreenTree3
  | GreenTree4
  | BrownTree1
  | BrownTree2
  | BrownTree3
  | BrownTree4
  | Rock1
  | Rock2
  | Rock3
  | Rock4
  | Bush1
  | Bush2
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
tileTypeChar GreenTree1     = 'G'
tileTypeChar GreenTree2     = 'G'
tileTypeChar GreenTree3     = 'G'
tileTypeChar GreenTree4     = 'G'
tileTypeChar BrownTree1     = 'B'
tileTypeChar BrownTree2     = 'B'
tileTypeChar BrownTree3     = 'B'
tileTypeChar BrownTree4     = 'B'
tileTypeChar Rock1          = 'R'
tileTypeChar Rock2          = 'R'
tileTypeChar Rock3          = 'R'
tileTypeChar Rock4          = 'R'
tileTypeChar Bush1          = 'b'
tileTypeChar Bush2          = 'b'
tileTypeChar Water          = 'W'

greenTrees :: [TileType]
greenTrees = [GreenTree1, GreenTree2, GreenTree3, GreenTree4]

brownTrees :: [TileType]
brownTrees = [BrownTree1, BrownTree2, BrownTree3, BrownTree4]

rocks :: [TileType]
rocks = [Rock1, Rock2, Rock3, Rock4]

bushes :: [TileType]
bushes = [Bush1, Bush2]