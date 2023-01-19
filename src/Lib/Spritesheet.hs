{-# LANGUAGE TupleSections #-}

module Lib.Spritesheet
  ( allFrames
  , allFramesFlipped
  , framePictures
  , framesIndexed
  , framesIndexedFlipped
  , genRowIndices
  , animFrames
  , animFramesFlip
  , Frame
  , FrameIndex
  ) where

import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import qualified Graphics.Gloss as G
import Graphics.Gloss (Picture)
import ThirdParty.GraphicsGlossJuicy (fromImageRGBA8)
import Codec.Picture.Extra (crop, flipHorizontally)
import Codec.Picture (DynamicImage, dynamicMap, Image (imageWidth, imageHeight), convertRGBA8, PixelRGBA8)

-- data Frame = Frame
--   { index    :: FrameIndex
--   , original :: Picture
--   , flipped  :: Picture
--   , size     :: FrameSize
--   }

type FrameIndex = (Int, Int)
type FrameSize = (Int, Int)
type Frame = (FrameIndex, Picture, FrameSize)
type RowIndexConfig = (Int, Int, Int)   -- (row index, col index, frame count)

type AllFramesFn = Int -> Int -> DynamicImage -> [Frame]

type CropFn = Int -> Int -> Int -> Int -> DynamicImage -> G.Picture

-- r and c are row and column of the frame in the spritesheet or tileset
cropFrameDyn :: Int -> Int -> Int -> Int -> DynamicImage -> Image PixelRGBA8
cropFrameDyn r c w h = crop (c * w) (r * h) w h . convertRGBA8

cropFrame :: Int -> Int -> Int -> Int -> DynamicImage -> G.Picture
cropFrame r c w h = fromImageRGBA8 . cropFrameDyn r c w h

cropFrameAndFlip :: Int -> Int -> Int -> Int -> DynamicImage -> G.Picture
cropFrameAndFlip r c w h = fromImageRGBA8 . flipHorizontally . cropFrameDyn r c w h

-- w and h are width and height of a single frame
allFramesFn :: CropFn -> Int -> Int -> DynamicImage -> [Frame]
allFramesFn cropFn w h img = 
  [((r, c), cropFn r c w h img, (w, h)) | r <- [0..hFr-1], c <- [0..wFr-1]]
  where
    wFr = dynamicMap imageWidth img `div` w
    hFr = dynamicMap imageHeight img `div` h

allFrames :: Int -> Int -> DynamicImage -> [Frame]
allFrames = allFramesFn cropFrame

allFramesFlipped :: Int -> Int -> DynamicImage -> [Frame]
allFramesFlipped = allFramesFn cropFrameAndFlip

framesIndexedFn :: AllFramesFn -> Int -> Int -> DynamicImage -> [FrameIndex] -> [Frame]
framesIndexedFn fsFn w h img = mapMaybe (frameMap HM.!?)
  where 
    frameKV f@(i, _, _) = (i, f)
    mapFromFrames = HM.fromList . map frameKV
    frameMap = mapFromFrames $ fsFn w h img

framesIndexed :: Int -> Int -> DynamicImage -> [FrameIndex] -> [Frame]
framesIndexed = framesIndexedFn allFrames

framesIndexedFlipped :: Int -> Int -> DynamicImage -> [FrameIndex] -> [Frame]
framesIndexedFlipped = framesIndexedFn allFramesFlipped

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

animFramesFlip :: FrameSize -> RowIndexConfig -> DynamicImage -> [Picture]
animFramesFlip (w, h) (r, c, cnt) img = 
  framePictures $ framesIndexedFlipped w h img $ genRowIndices r c cnt