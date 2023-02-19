module Lib.Level.PathSegment
  ( PathSegment
  , CornerType(..)
  , haveOverlap
  , areAdjacent
  , crossing
  , cornerType
  , isEdge
  ) where

import Lib.Level.Grid (gridCols, gridRows)
import Lib.Level.Point (Point)
import Lib.Util (inRangeAbsExcl)

type PathSegment = (Point, Point)
data CornerType = TopLeft | TopRight | BottomLeft | BottomRight
  deriving (Show, Eq)

-- | Checks whether two ranges overlap. Inputs don't have to be sorted.
rangeOverlap :: (Int, Int) -> (Int, Int) -> Bool
rangeOverlap (a1, a2) (b1, b2)
  | a1 > a2   = rangeOverlap (a2, a1) (b1, b2)
  | b1 > b2   = rangeOverlap (a1, a2) (b2, b1)
  | a1 > b1   = rangeOverlap (b1, b2) (a1, a2)
  | a2 <= b1  = False
  | otherwise = True

-- | Checks whether two path segments overlap
haveOverlap :: PathSegment -> PathSegment -> Bool
haveOverlap ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
  | all (== x1) [x2, x3, x4] = rangeOverlap (y1, y2) (y3, y4)
  | all (== y1) [y2, y3, y4] = rangeOverlap (x1, x2) (x3, x4)
  | otherwise                = False

-- | Checks whether two segments are adjacent. Segments are adjacent if they
-- are parallel and are next to each other.
areAdjacent :: PathSegment -> PathSegment -> Bool
areAdjacent ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
  x1 == x2 && x3 == x4 && abs (x1 - x3) == 1 && rangeOverlap (y1, y2) (y3, y4) ||
  y1 == y2 && y3 == y4 && abs (y1 - y3) == 1 && rangeOverlap (x1, x2) (x3, x4)
  
-- There might be a bug here
-- | Finds a crossing point between two segments if there is one
crossing :: PathSegment -> PathSegment -> Maybe Point
crossing ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
  | x1 == x2 && y3 == y4 && inRangeAbsExcl (x3, x4) x1 && inRangeAbsExcl (y1, y2) y3 = Just (x1, y3)
  | x3 == x4 && y1 == y2 && inRangeAbsExcl (x1, x2) x3 && inRangeAbsExcl (y3, y4) y1 = Just (x3, y1)
  | otherwise = Nothing

-- | Finds a corner between two segments if there is one
cornerType :: PathSegment -> PathSegment -> Maybe (Point, CornerType)
cornerType (a1, b1) (a2, b2)
  | a1 == a2 && b1 == b2 || a1 == b2 && b1 == a2 = Nothing
  | a1 == a2 = getCornerType b1 a1 b2
  | a1 == b2 = getCornerType b1 a1 a2
  | b1 == a2 = getCornerType a1 b1 b2
  | b1 == b2 = getCornerType a1 b1 a2
  | otherwise = Nothing
  where
    getCornerType (x1, y1) p@(x2, y2) (x3, y3)
      | x1 == x3 || y1 == y3 = Nothing    -- Not a corner (straight line)
      | x1 > x3              = getCornerType (x3, y3) (x2, y2) (x1, y1)
      | x1 < x2  && y3 > y2  = Just (p, TopRight)
      | x1 < x2  && y3 < y2  = Just (p, BottomRight)
      | x1 == x2 && y1 < y2  = Just (p, BottomLeft)
      | x1 == x2 && y1 > y2  = Just (p, TopLeft)
      | otherwise            = Nothing

isEdge :: PathSegment -> Bool
isEdge ((x1, y1), (x2, y2)) = isVerticalEdge || isHorizontalEdge
  where
    isVerticalEdge = x1 == x2 && (x1 == 0 || x1 == gridCols - 1)
    isHorizontalEdge = y1 == y2 && (y1 == 0 || y1 == gridRows - 1)
