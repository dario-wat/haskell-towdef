{-# LANGUAGE NamedFieldPuns #-}

module GameObjects.Enemy
  ( Enemy(..)
  , update
  , draw
  , mkEnemy
  , mkFromGridPath
  ) where

import qualified Graphics.Gloss as G
import qualified GameObjects.GameObject as GO
import qualified GameObjects.Sprite as S
import Lib.Util (distance)
import qualified Lib.Level.Path as P

-- TODO
-- 1. Enemy types
-- 2. Enemy health
-- 3. All enemy logic
-- 4. Enemy follow path

data Enemy = Enemy
  { sprite :: S.Sprite
  , remainingPath :: G.Path
  } deriving (Show)

instance GO.GameObject Enemy where
  update = update
  draw   = draw

speed :: Float
speed = 3

distanceThreshold :: Float
distanceThreshold = 3

mkFromGridPath :: S.Sprite -> P.Path -> Enemy
mkFromGridPath sprite path = mkEnemy sprite $ P.toGlossPath path

mkEnemy :: S.Sprite -> G.Path -> Enemy
mkEnemy sprite path = Enemy { sprite, remainingPath = path }

-- | Jsut movement for now
update :: Float -> Enemy -> Enemy
update time enemy = moveTowardsNextPoint $ enemy { sprite = S.update time (sprite enemy) }

draw :: Float -> Enemy -> G.Picture
draw time Enemy{sprite} = S.draw time sprite

moveTowardsNextPoint :: Enemy -> Enemy
moveTowardsNextPoint enemy@Enemy{remainingPath=[]} = enemy
moveTowardsNextPoint enemy@Enemy{sprite, remainingPath}
  | distance (x, y) (S.x sprite, S.y sprite) < distanceThreshold =
    enemy 
      { sprite = sprite { S.x = x, S.y = y } 
      , remainingPath = tail remainingPath
      }
  | otherwise = 
      enemy
        { sprite = sprite { S.velX = velX , S.velY = velY }
        }
  where
    (x, y) = head remainingPath
    angle = atan2 (y - S.y sprite) (x - S.x sprite)
    velX = speed * cos angle
    velY = speed * sin angle
