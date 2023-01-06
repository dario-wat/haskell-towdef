{-# LANGUAGE NamedFieldPuns #-}

module GameObjects.Sprite
  ( draw
  , mkSprite
  , mkDefaultSprite
  , Sprite(..)
  ) where

import Graphics.Gloss (Picture, translate)

data Sprite = Sprite
  { x :: Float
  , y :: Float
  , pic :: Picture
  }

draw :: Sprite -> Picture
draw (Sprite x y pic) = translate x y pic

mkSprite :: Float -> Float -> Picture -> Sprite
mkSprite x y pic = Sprite {x, y, pic}

mkDefaultSprite :: Picture -> Sprite
mkDefaultSprite = mkSprite 0 0
