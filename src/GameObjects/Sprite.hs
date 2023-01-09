{-# LANGUAGE NamedFieldPuns #-}

module GameObjects.Sprite
  ( draw
  , update
  , mkSprite
  , mkNonAnimatedSprite
  , mkStaticSprite
  , mkDefaultSprite
  , Sprite(..)
  ) where

import Graphics.Gloss (Picture, translate, blank)
import Lib.Animation (MkAnimation, mkNoAnimation)

data Sprite = Sprite
  { x      :: Float
  , y      :: Float
  , velX   :: Float
  , velY   :: Float
  , pic    :: Picture
  , mkAnim :: MkAnimation
  }

draw :: Sprite -> Picture
draw (Sprite x y _ _ pic _) = translate x y pic

update :: Sprite -> Sprite
update (Sprite x y velX velY pic mkAnim) = Sprite x' y' velX velY pic mkAnim
  where
    x' = x + velX
    y' = y + velY

mkSprite :: Float -> Float -> Float -> Float -> Picture -> MkAnimation -> Sprite
mkSprite x y velX velY pic mkAnim = Sprite {x, y, velX, velY, pic, mkAnim}

mkNonAnimatedSprite :: Float -> Float -> Float -> Float -> Picture -> Sprite
mkNonAnimatedSprite x y velX velY pic = mkSprite x y velX velY pic mkNoAnimation

mkStaticSprite :: Float -> Float -> Picture -> Sprite
mkStaticSprite x y pic = mkSprite x y 0 0 pic mkNoAnimation

mkDefaultSprite :: Sprite
mkDefaultSprite = mkSprite 0 0 0 0 blank mkNoAnimation

-- repeatSpriteAnimation :: Sprite -> MkAnimation -> Float -> Sprite
-- repeatSpriteAnimation (Sprite x y pic _) newAnim t = Sprite x y pic (newAnim t)