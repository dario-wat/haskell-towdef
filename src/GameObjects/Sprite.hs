{-# LANGUAGE NamedFieldPuns #-}

module GameObjects.Sprite
  ( draw
  , update
  , mkSprite
  , mkNonAnimatedSprite
  , mkAnimatedSprite
  , mkStaticSprite
  , mkDefaultSprite
  , Sprite(..)
  , Visual(..)
  ) where

-- TODO WIP

import qualified Graphics.Gloss as G
import qualified Lib.Animation as A
import qualified ThirdParty.GraphicsGlossGame as G

data Visual = Pic G.Picture | Anim A.Animation

data Sprite = Sprite
  { x      :: !Float
  , y      :: !Float
  , velX   :: !Float
  , velY   :: !Float
  , vis    :: !Visual
  }

-- type SpritePic = Sprite G.Picture
-- type SpriteAnim = Sprite A.Animation

-- draw :: Sprite -> G.Scene world
-- draw Sprite{x, y, vis=Pic pic} = G.translating (const (x, y)) $ G.picturing (const pic)
-- draw Sprite{x, y, vis=Anim anim} = G.translating (const (x, y)) $ A.animating (const anim)

draw :: Sprite -> Float -> G.Picture
draw Sprite{x, y, vis=Pic pic}   _    = G.translate x y pic
draw Sprite{x, y, vis=Anim anim} time = G.translate x y $ A.draw anim time

-- spriting :: (world -> Sprite) -> G.Scene world
-- spriting worldToSprite = A.animating (anim . worldToSprite)
--   where worldToPos world = (x $ worldToSprite world, y $ worldToSprite world)

update :: Float -> Sprite -> Sprite
update _ Sprite{x, y, velX, velY, vis=Pic pic} = Sprite 
  { x = x + velX
  , y = y + velY
  , velX
  , velY
  , vis = Pic pic
  }
update time Sprite{x, y, velX, velY, vis=Anim anim} = Sprite 
  { x = x + velX
  , y = y + velY
  , velX
  , velY
  , vis = Anim $ A.update time anim
  }

mkSprite :: Float -> Float -> Float -> Float -> Visual -> Sprite
mkSprite x y velX velY vis = Sprite 
  { x
  , y
  , velX
  , velY
  , vis
  }

mkDefaultSprite :: Sprite
mkDefaultSprite = mkSprite 0 0 0 0 (Pic G.blank)

mkNonAnimatedSprite :: Float -> Float -> Float -> Float -> G.Picture -> Sprite
mkNonAnimatedSprite x y velX velY pic = mkSprite x y velX velY (Pic pic)

mkStaticSprite :: Float -> Float -> G.Picture -> Sprite
mkStaticSprite x y pic = mkSprite x y 0 0 (Pic pic)

mkAnimatedSprite :: Float -> Float -> A.Animation -> Sprite
mkAnimatedSprite x y anim = mkSprite x y 0 0 (Anim anim)