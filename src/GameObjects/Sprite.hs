{-# LANGUAGE NamedFieldPuns #-}

module GameObjects.Sprite
  ( Sprite(..)
  , Visual(..)
  , draw
  , update
  , mkSprite
  , mkNonAnimatedSprite
  , mkAnimatedSprite
  , mkStaticSprite
  , mkDefaultSprite
  ) where

import qualified Graphics.Gloss as G
import qualified GameObjects.GameObject as GO
import qualified Lib.Animation as A

data Visual = Pic G.Picture | Anim A.Animation

data Sprite = Sprite
  { x    :: !Float
  , y    :: !Float
  , velX :: !Float
  , velY :: !Float
  , vis  :: !Visual
  }

instance Show Sprite where
  show Sprite{x, y, velX, velY} = 
    "Sprite " ++
    "{ x = " ++ show x ++ 
    ", y = " ++ show y ++ 
    ", velX = " ++ show velX ++ 
    ", velY = " ++ show velY ++ 
    " }"

instance GO.GameObject Sprite where
  update = update
  draw   = draw

draw :: Float -> Sprite -> G.Picture
draw _    Sprite{x, y, vis=Pic pic}   = G.translate x y pic
draw time Sprite{x, y, vis=Anim anim} = G.translate x y $ A.draw time anim

update :: Float -> Sprite -> Sprite
update _ Sprite{x, y, velX, velY, vis=Pic pic} = Sprite 
  { x    = x + velX
  , y    = y + velY
  , velX
  , velY
  , vis  = Pic pic
  }
update time Sprite{x, y, velX, velY, vis=Anim anim} = Sprite 
  { x    = x + velX
  , y    = y + velY
  , velX
  , velY
  , vis  = Anim $ A.update time anim
  }

mkSprite :: Float -> Float -> Float -> Float -> Visual -> Sprite
mkSprite x y velX velY vis = Sprite { x, y, velX, velY, vis }

mkDefaultSprite :: Sprite
mkDefaultSprite = mkSprite 0 0 0 0 (Pic G.blank)

mkNonAnimatedSprite :: Float -> Float -> Float -> Float -> G.Picture -> Sprite
mkNonAnimatedSprite x y velX velY pic = mkSprite x y velX velY (Pic pic)

mkStaticSprite :: Float -> Float -> G.Picture -> Sprite
mkStaticSprite x y pic = mkSprite x y 0 0 (Pic pic)

mkAnimatedSprite :: Float -> Float -> A.Animation -> Sprite
mkAnimatedSprite x y anim = mkSprite x y 0 0 (Anim anim)