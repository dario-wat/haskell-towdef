{-# LANGUAGE TupleSections #-}
module Lib.Spritesheet
  ( frames
  , framesWithCoords
  , framesIndexed
  , genRowIndices
  , Frame
  , FrameIndex
  ) where

import Graphics.Gloss (Picture)
import Codec.Picture (DynamicImage)
import Lib.Image (dynWidth, dynHeight, cropFrame)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)

type FrameIndex = (Int, Int)
type Frame = (FrameIndex, Picture)

-- w and h are width and height of a single frame
framesWithCoords :: Int -> Int -> DynamicImage -> [Frame]
framesWithCoords w h img = 
  [((r, c), cropFrame r c w h img) | r <- [0..hFr-1], c <- [0..wFr-1]]
  where
    wFr = dynWidth img `div` w
    hFr = dynHeight img `div` h

frames :: Int -> Int -> DynamicImage -> [Picture]
frames w h = map snd . framesWithCoords w h

framesIndexed :: Int -> Int -> [FrameIndex] -> DynamicImage -> [Picture]
framesIndexed w h coords img = mapMaybe picFromMap coords
  where
    frameMap = HM.fromList $ framesWithCoords w h img
    picFromMap c = frameMap HM.!? c

genRowIndices :: Int -> Int -> Int -> [FrameIndex]
genRowIndices r c w = map (r,) [c..c+w]