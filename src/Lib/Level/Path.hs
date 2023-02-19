{-# LANGUAGE BangPatterns #-}

module Lib.Level.Path 
  ( Path      -- TODO this shouldnt be exported
  , toGlossPath
  , hasAdjacentSegment
  , pathLength
  , hasOverlap
  , crossingCount
  , quadrantCount
  , pathSegments
  , createAllPaths
  , allSegmentPairs
  ) where

import Data.List (group, nub)
import Data.Maybe (isJust)
import qualified Graphics.Gloss as GL
import qualified Lib.Level.Grid as G
import qualified Lib.Level.PathSegment as S
import qualified Lib.Level.Point as P
import Lib.Util (cartProd, manhattanDist, count)

type Path = [P.Point]

toGlossPath :: Path -> GL.Path
toGlossPath = map (`G.gridCenterOf` (1, 1))

hasAdjacentSegment :: Path -> Bool
hasAdjacentSegment = any (uncurry S.areAdjacent) . allSegmentPairs

pathLength :: Path -> Int
pathLength = (+1) . sum . map (uncurry manhattanDist) . pathSegments

hasOverlap :: Path -> Bool
hasOverlap = any (uncurry S.haveOverlap) . allSegmentPairs

crossingCount :: Path -> Int
crossingCount = count (isJust . uncurry S.crossing) . allSegmentPairs

quadrantCount :: Path -> Int
quadrantCount = length . nub . map P.quadrant

pathSegments :: Path -> [S.PathSegment]
pathSegments path = zip path (tail path)

allSegmentPairs :: Path -> [(S.PathSegment, S.PathSegment)]
allSegmentPairs path = filter (uncurry (/=)) $ cartProd allSegments allSegments
  where allSegments = pathSegments path


-------------------------------------------------------------------------------
-- Path creation
-------------------------------------------------------------------------------

-- | There are either one or two paths between two points.
-- There is only one path if the points are on the same row or column.
-- Otherwise there are two paths, one going up and one going right.
connectTwoPoints :: P.Point -> P.Point -> [Path]
connectTwoPoints (x1, y1) (x2, y2)
  | x1 == x2 || y1 == y2 = [singlePath]
  | otherwise            = [path1, path2]
  where
    path1 = [(x1, y1), (x1, y2), (x2, y2)]
    path2 = [(x1, y1), (x2, y1), (x2, y2)]
    singlePath = [(x1, y1), (x2, y2)]

-- | Creates a list of paths for each adjacent pair of points
connectAllPoints :: [P.Point] -> [[Path]]
connectAllPoints []         = []
connectAllPoints [_]        = []
connectAllPoints (p1:p2:ps) = connectTwoPoints p1 p2 : connectAllPoints (p2:ps)

-- -- | Combines all path lists created by connectAllPoints recursively
combinePaths :: [[Path]] -> [Path]
combinePaths = combinePathsAcc [[]]

combinePathsAcc :: [Path] -> [[Path]] -> [Path]
combinePathsAcc !acc []           = acc
combinePathsAcc !acc ([p]:ps)     = combinePathsAcc (map (++p) acc) ps
combinePathsAcc !acc ([p1,p2]:ps) = combinePathsAcc (map (++p1) acc ++ map (++p2) acc) ps
combinePathsAcc _   _             = error "combinePathsAcc: impossible"

createAllPaths :: [P.Point] -> [Path]
createAllPaths = map removeConsecutiveDuplicates . combinePaths . connectAllPoints
  where removeConsecutiveDuplicates = map head . group
