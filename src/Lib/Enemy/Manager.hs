{-# LANGUAGE NamedFieldPuns #-}

module Lib.Enemy.Manager 
  ( EnemyManager(..)
  , update
  , draw
  , mkEnemyManager
  ) where

import Data.HashMap.Strict ((!))
import Graphics.Gloss (Picture, pictures)
import qualified GameObjects.Enemy as E
import Lib.Enemy.Animations (EnemyAnimationsMap)
import Lib.Enemy.Types (EnemyType)
import Lib.Level.Path (Path)

-- TODO
-- Kill enemy when it reaches the last point in the path
-- Will probably need to shift the spawn list by the level start time

-- | Enemy type to spawn and the time from the begginning of the level
type SpawnList = [(EnemyType, Float)]

data EnemyManager = EnemyManager
  { enemyAnimations :: !EnemyAnimationsMap
  , spawnList       :: !SpawnList
  , path            :: !Path
  , enemies         :: ![E.Enemy]
  }

mkEnemyManager :: EnemyAnimationsMap -> SpawnList -> Path -> EnemyManager
mkEnemyManager enemyAnimations spawnList path = EnemyManager
  { enemyAnimations
  , spawnList
  , path
  , enemies = []
  }

draw :: Float -> EnemyManager -> Picture
draw time EnemyManager{enemies} = pictures $ map (E.draw time) enemies

update :: Float -> EnemyManager -> EnemyManager
update time = updateEnemies time . updateSpawn time

updateSpawn :: Float -> EnemyManager -> EnemyManager
updateSpawn _    enemyManager@EnemyManager{spawnList=[]} = enemyManager
updateSpawn time enemyManager@EnemyManager
                   { spawnList=((enemyType, spawnTime):rest)
                   , enemyAnimations
                   , path
                   , enemies
                   }
  | spawnTime > time = enemyManager
  | otherwise        = enemyManager
      { spawnList = rest
      , enemies   = E.mkFromGridPath path (enemyAnimations ! enemyType) : enemies
      }

updateEnemies :: Float -> EnemyManager -> EnemyManager
updateEnemies time enemyManager@EnemyManager{enemies} = enemyManager 
  { enemies = map (E.update time) enemies }
