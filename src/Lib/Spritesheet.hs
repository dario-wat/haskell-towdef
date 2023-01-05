{-# LANGUAGE TupleSections #-}
module Lib.Spritesheet
  ( frames
  , framePictures
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
type FrameSize = (Int, Int)
type Frame = (FrameIndex, Picture, FrameSize)

-- w and h are width and height of a single frame
frames :: Int -> Int -> DynamicImage -> [Frame]
frames w h img = 
  [((r, c), cropFrame r c w h img, (w, h)) | r <- [0..hFr-1], c <- [0..wFr-1]]
  where
    wFr = dynWidth img `div` w
    hFr = dynHeight img `div` h

framePictures :: Int -> Int -> DynamicImage -> [Picture]
framePictures w h = map framePicture . frames w h

framesIndexed :: Int -> Int -> DynamicImage -> [FrameIndex] -> [Frame]
framesIndexed w h img = mapMaybe (frameMap HM.!?)
  where frameMap = mapFromFrames $ frames w h img

genRowIndices :: Int -> Int -> Int -> [FrameIndex]
genRowIndices r c w = map (r,) [c..c+w]

framePicture :: Frame -> Picture
framePicture (_, pic, _) = pic

frameKV :: Frame -> (FrameIndex, Frame)
frameKV f@(i, _, _) = (i, f)

mapFromFrames :: [Frame] -> HM.HashMap FrameIndex Frame
mapFromFrames = HM.fromList . map frameKV