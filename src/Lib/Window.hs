module Lib.Window
  ( windowSize
  , windowWidth
  , windowHeight
  , windowPosition
  , windowTopLeft
  , windowTopRight
  , windowBottomLeft
  , windowBottomRight
  ) where

windowSize :: (Int, Int)
windowSize = (1440, 810)
-- windowSize = (1920, 1080)  -- only for fullscreen

windowWidth :: Int
windowWidth = fst windowSize

windowHeight :: Int
windowHeight = snd windowSize

windowPosition :: (Int, Int)
windowPosition = (0, 0)

halfWindowSize :: (Float, Float)
halfWindowSize = (x, y)
  where
    x = fromIntegral $ fst windowSize `quot` 2
    y = fromIntegral $ snd windowSize `quot` 2

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