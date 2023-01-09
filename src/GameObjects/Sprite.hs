{-# LANGUAGE NamedFieldPuns #-}

module GameObjects.Sprite
  ( draw
  , mkSprite
  , mkStaticSprite
  , mkDefaultSprite
  , Sprite(..)
  ) where

import Graphics.Gloss (Picture, translate, blank)
import Lib.Animation (MkAnimation, mkNoAnimation)

data Sprite = Sprite
  { x    :: Float
  , y    :: Float
  , pic  :: Picture
  , mkAnim :: MkAnimation
  }

draw :: Sprite -> Picture
draw (Sprite x y pic _) = translate x y pic

mkSprite :: Float -> Float -> Picture -> MkAnimation -> Sprite
mkSprite x y pic mkAnim = Sprite {x, y, pic, mkAnim}

mkStaticSprite :: Float -> Float -> Picture -> Sprite
mkStaticSprite x y pic = mkSprite x y pic mkNoAnimation

mkDefaultSprite :: Sprite
mkDefaultSprite = mkSprite 0 0 blank mkNoAnimation

-- repeatSpriteAnimation :: Sprite -> MkAnimation -> Float -> Sprite
-- repeatSpriteAnimation (Sprite x y pic _) newAnim t = Sprite x y pic (newAnim t)