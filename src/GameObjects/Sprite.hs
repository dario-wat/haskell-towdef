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

-- TODO WIP

import Graphics.Gloss (Picture, translate, blank)
import Lib.Animation (MkAnimation)
import ThirdParty.GraphicsGlossGame (noAnimation)

data Sprite = Sprite
  { x      :: Float
  , y      :: Float
  , velX   :: Float
  , velY   :: Float
  , pic    :: Picture
  , mkAnim :: MkAnimation   -- TODO what is this?
  }

draw :: Sprite -> Picture
draw (Sprite x y _ _ pic _) = translate x y pic

update :: Sprite -> Sprite
update (Sprite x y velX velY pic mkAnim) = Sprite x' y' velX velY pic mkAnim
  where
    x' = x + velX
    y' = y + velY

-- TODO should be removed
mkNoAnimationFn :: MkAnimation
mkNoAnimationFn _ = noAnimation

mkSprite :: Float -> Float -> Float -> Float -> Picture -> MkAnimation -> Sprite
mkSprite x y velX velY pic mkAnim = Sprite {x, y, velX, velY, pic, mkAnim}

mkNonAnimatedSprite :: Float -> Float -> Float -> Float -> Picture -> Sprite
mkNonAnimatedSprite x y velX velY pic = mkSprite x y velX velY pic mkNoAnimationFn

mkStaticSprite :: Float -> Float -> Picture -> Sprite
mkStaticSprite x y pic = mkSprite x y 0 0 pic mkNoAnimationFn

mkDefaultSprite :: Sprite
mkDefaultSprite = mkSprite 0 0 0 0 blank mkNoAnimationFn

-- repeatSpriteAnimation :: Sprite -> MkAnimation -> Float -> Sprite
-- repeatSpriteAnimation (Sprite x y pic _) newAnim t = Sprite x y pic (newAnim t)