module Lib.Spritesheet
  ( frames
  , framesWithCoords
  ) where

import Graphics.Gloss (Picture)
import Codec.Picture (DynamicImage)
import Lib.Image (dynWidth, dynHeight, cropFrame)

-- w and h are width and height of a single frame
framesWithCoords :: Int -> Int -> DynamicImage -> [(Picture, (Int, Int))]
framesWithCoords w h img = 
  [(cropFrame r c w h img, (r, c)) | r <- [0..hFr-1], c <- [0..wFr-1]]
  where
    wFr = dynWidth img `div` w
    hFr = dynHeight img `div` h

frames :: Int -> Int -> DynamicImage -> [Picture]
frames w h = map fst . framesWithCoords w h