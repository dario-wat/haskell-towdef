{-# LANGUAGE NamedFieldPuns #-}

module Lib.Enemy.Spawner 
  (
    mkEnemySpawner
  ) where

import GameObjects.Enemy (Enemy)
import Lib.Enemy.Animations (EnemyAnimationsMap)
import Lib.Level.Path (Path)

data EnemySpawner = EnemySpawner
  { enemyAnimations :: EnemyAnimationsMap
  , times :: [Float]
  , path :: Path
  }

mkEnemySpawner :: EnemyAnimationsMap -> [Float] -> Path -> EnemySpawner
mkEnemySpawner enemyAnimations times path = EnemySpawner 
  { enemyAnimations
  , times
  , path
  }

-- maybeSpawn :: EnemySpawner -> Float -> Maybe Enemy
-- maybeSpawn EnemySpawner{times=[]} time = Nothing
-- maybeSpawn EnemySpawner{times=(t:ts), path, enemyAnimations} time
--   | time >= t = Just $ mkEnemy path enemyAnimations
--   | otherwise = Nothing
  