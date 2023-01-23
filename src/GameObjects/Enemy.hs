{-# LANGUAGE NamedFieldPuns #-}

module GameObjects.Enemy
  ( Enemy(..)
  , update
  , draw
  , mkFromGridPath
  ) where

import Prelude hiding (Left, Right)
import qualified Graphics.Gloss as G
import qualified GameObjects.Sprite as S
import qualified Lib.Animation as A
import qualified Lib.Enemy.Animations as E
import qualified Lib.Level.Path as P
import Lib.Util (distance, angle, inRangeF, headOrDefault)

-- TODO
-- 1. Enemy types
-- 2. Enemy health
-- 3. All enemy logic

data Direction = Left | Right | Up | Down 
  deriving (Show, Eq)

data Enemy = Enemy
  { sprite        :: !S.Sprite
  , remainingPath :: !G.Path
  , animationSet  :: !E.EnemyAnimations
  , direction     :: !Direction
  } 

instance Show Enemy where
  show Enemy{sprite, remainingPath, direction} = 
    "Enemy " ++
    "{ sprite = " ++ show sprite ++ 
    ", remainingPath = " ++ show remainingPath ++ 
    ", direction = " ++ show direction ++
    " }"

speed :: Float
speed = 3

distanceThreshold :: Float
distanceThreshold = 3

mkFromGridPath :: P.Path -> E.EnemyAnimations -> Enemy
mkFromGridPath path = mkEnemy (P.toGlossPath path)

mkEnemy :: G.Path -> E.EnemyAnimations -> Enemy
mkEnemy path animationSet = Enemy 
  { sprite        = S.mkAnimatedSprite x y $ directionAnimation Right animationSet
  , remainingPath = path
  , animationSet
  , direction     = Right 
  }
  where (x, y) = headOrDefault (0, 0) path

update :: Float -> Enemy -> Enemy
update time = updateEnemySprite time 
  . faceDirectionOfMovement 
  . moveTowardsNextPoint 

draw :: Float -> Enemy -> G.Picture
draw time Enemy{sprite} = S.draw time sprite

updateEnemySprite :: Float -> Enemy -> Enemy
updateEnemySprite time enemy@Enemy{sprite} = enemy { sprite = S.update time sprite }

moveTowardsNextPoint :: Enemy -> Enemy
moveTowardsNextPoint enemy@Enemy{remainingPath=[]} = enemy
moveTowardsNextPoint enemy@Enemy{sprite, remainingPath}
  | shouldSnap = enemy 
      { sprite = sprite { S.x = x, S.y = y } 
      , remainingPath = tail remainingPath
      }
  | otherwise = enemy
      { sprite = sprite { S.velX = velX , S.velY = velY }
      }
  where
    (x, y) = head remainingPath
    shouldSnap = distance (x, y) (S.x sprite, S.y sprite) < distanceThreshold
    a = angle (S.x sprite, S.y sprite) (x, y)
    (velX, velY) = (speed * cos a, speed * sin a)

faceDirectionOfMovement :: Enemy -> Enemy
faceDirectionOfMovement enemy@Enemy{sprite, animationSet, direction} 
  | newDirection == direction = enemy
  | otherwise                 = enemy
      { sprite = sprite { S.vis = S.Anim $ directionAnimation newDirection animationSet } 
      , direction = newDirection
      }
  where
    movementAngle = atan2 (S.velY sprite) (S.velX sprite)
    newDirection
      | inRangeF (-5*pi/4, -3*pi/4) movementAngle = Left
      | inRangeF (-3*pi/4,   -pi/4) movementAngle = Down
      | inRangeF (  -pi/4,    pi/4) movementAngle = Right
      | inRangeF (   pi/4,  3*pi/4) movementAngle = Up
      | inRangeF ( 3*pi/4,  5*pi/4) movementAngle = Left  -- Hack to fix rounding errors
      | otherwise                                 = error "impossible angle"

directionAnimation :: Direction -> E.EnemyAnimations -> A.Animation
directionAnimation Right = A.mkInfAnimation . E.moveRight 
directionAnimation Left  = A.mkInfAnimation . E.moveLeft  
directionAnimation Up    = A.mkInfAnimation . E.moveUp    
directionAnimation Down  = A.mkInfAnimation . E.moveDown