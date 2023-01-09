{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Lib.Window where

import Data.Bifunctor (Bifunctor(bimap))

windowSize :: (Float, Float)
windowSize = (1440, 810)
-- windowSize = (1920, 1080)  -- only for fullscreen

windowSizeForInWindow :: (Int, Int)
windowSizeForInWindow = bimap round round windowSize

windowWidth :: Float
windowWidth = fst windowSize

windowHeight :: Float
windowHeight = snd windowSize

windowPosition :: (Float, Float)
windowPosition = (0, 0)

windowPositionForInWindow :: (Int, Int)
windowPositionForInWindow = bimap round round windowPosition

halfWindowSize :: (Float, Float)
halfWindowSize = (fst windowSize / 2, snd windowSize / 2)

halfWindowWidth :: Float
halfWindowWidth = fst halfWindowSize

halfWindowHeight :: Float
halfWindowHeight = snd halfWindowSize

windowTopLeft :: (Float, Float)
windowTopLeft = (-x, y)
  where (x, y) = halfWindowSize

windowTopRight :: (Float, Float)
windowTopRight = (x, y)
  where (x, y) = halfWindowSize

-- | Bottom left corner of the window is the origin (minimum x and y values)
windowBottomLeft :: (Float, Float)
windowBottomLeft = (-x, -y)
  where (x, y) = halfWindowSize

windowBottomRight :: (Float, Float)
windowBottomRight = (x, -y)
  where (x, y) = halfWindowSize

-- x' and y' are coordinates relative to the bottom left corner of the window
x' :: Float -> Float
x' x = x - halfWindowWidth

y' :: Float -> Float
y' y = y - halfWindowHeight