{-# LANGUAGE TupleSections #-}
module Lib.Spritesheet
  ( allFrames
  , framePictures
  , framesIndexed
  , genRowIndices
  , animFrames
  , Frame
  , FrameIndex
  ) where

import Graphics.Gloss (Picture)
import Codec.Picture (DynamicImage)
import Lib.Image (dynWidth, dynHeight, cropFrame)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)

type FrameIndex = (Int, Int)
type FrameSize = (Int, Int)
type Frame = (FrameIndex, Picture, FrameSize)
type RowIndexConfig = (Int, Int, Int)   -- (row index, col index, frame count)

-- w and h are width and height of a single frame
allFrames :: Int -> Int -> DynamicImage -> [Frame]
allFrames w h img = 
  [((r, c), cropFrame r c w h img, (w, h)) | r <- [0..hFr-1], c <- [0..wFr-1]]
  where
    wFr = dynWidth img `div` w
    hFr = dynHeight img `div` h

framesIndexed :: Int -> Int -> DynamicImage -> [FrameIndex] -> [Frame]
framesIndexed w h img = mapMaybe (frameMap HM.!?)
  where 
    frameKV f@(i, _, _) = (i, f)
    mapFromFrames = HM.fromList . map frameKV
    frameMap = mapFromFrames $ allFrames w h img

genRowIndices :: Int -> Int -> Int -> [FrameIndex]
genRowIndices r c cnt = map (r,) [c..c+cnt-1]

framePictures :: [Frame] -> [Picture]
framePictures = map framePicture
  where framePicture (_, pic, _) = pic

-- | Generate a list of pictures from a spritesheet given the frame size
-- and the row configuration (row index, column index, number of frames)
animFrames :: FrameSize -> RowIndexConfig -> DynamicImage -> [Picture]
animFrames (w, h) (r, c, cnt) img = 
  framePictures $ framesIndexed w h img $ genRowIndices r c cnt