{-# LANGUAGE TupleSections #-}

module Lib.Spritesheet
  ( animFrames
  , animFramesFlip
  ) where

import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import Codec.Picture.Extra (crop, flipHorizontally)
import qualified Codec.Picture as C
import qualified Graphics.Gloss as G
import ThirdParty.GraphicsGlossJuicy (fromImageRGBA8)

type FrameIndex = (Int, Int)
type FrameSize = (Int, Int)
type RowIndexConfig = (Int, Int, Int) -- (row index, col index, frame count in the row)

data Frame = Frame
  { index    :: FrameIndex
  , original :: G.Picture
  , flipped  :: G.Picture
  , size     :: FrameSize
  }

mkFrame :: FrameIndex -> FrameSize -> C.DynamicImage -> Frame
mkFrame i s img = Frame
  { index    = i
  , original = cropFrame i s img
  , flipped  = cropFrameAndFlip i s img
  , size     = s
  }

-- r and c are row and column of the frame in the spritesheet or tileset
cropFrameDyn :: FrameIndex -> FrameSize -> C.DynamicImage -> C.Image C.PixelRGBA8
cropFrameDyn (r, c) (w, h) = crop (c * w) (r * h) w h . C.convertRGBA8

cropFrame :: FrameIndex -> FrameSize -> C.DynamicImage -> G.Picture
cropFrame i s = fromImageRGBA8 . cropFrameDyn i s

cropFrameAndFlip :: FrameIndex -> FrameSize -> C.DynamicImage -> G.Picture
cropFrameAndFlip i s = fromImageRGBA8 . flipHorizontally . cropFrameDyn i s

-- | Creates a list of all frames in a spritesheet. Includes original and flipped.
-- w and h are width and height of a single frame
allFrames :: FrameSize -> C.DynamicImage -> [Frame]
allFrames (w, h) img = [ mkFrame (r, c) (w, h) img | r <- [0..hFr-1], c <- [0..wFr-1]]
  where
    wFr = C.dynamicMap C.imageWidth img `div` w
    hFr = C.dynamicMap C.imageHeight img `div` h

-- | Extracts all frames from a spritesheet given frame width, height and 
-- indices (row, column)
framesIndexed :: FrameSize -> C.DynamicImage -> [FrameIndex] -> [Frame]
framesIndexed s img = mapMaybe (frameMap HM.!?)
  where 
    frameKV frame = (index frame, frame)
    mapFromFrames = HM.fromList . map frameKV
    frameMap = mapFromFrames $ allFrames s img

genRowIndices :: RowIndexConfig -> [FrameIndex]
genRowIndices (r, c, cnt) = map (r,) [c..c+cnt-1]

-- | Generate a list of pictures from a spritesheet given the frame size
-- and the row configuration (row index, column index, number of frames)
animFrames :: FrameSize -> RowIndexConfig -> C.DynamicImage -> [G.Picture]
animFrames s conf img = map original $ framesIndexed s img $ genRowIndices conf

animFramesFlip :: FrameSize -> RowIndexConfig -> C.DynamicImage -> [G.Picture]
animFramesFlip s conf img = map flipped $ framesIndexed s img $ genRowIndices conf