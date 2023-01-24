{-# LANGUAGE TupleSections #-}

module Lib.Enemy.Animations
  ( EnemyAnimations(..)
  , EnemyAnimationsMap
  , firebugAnimations
  , leafbugAnimations
  , magmaCrabAnimations
  , scorpionAnimations
  , clampbeetleAnimations
  , firewaspAnimations
  , flyingLocustAnimations
  , voidbutterflyAnimations
  , allEnemyAnimations
  ) where

import Data.HashMap.Strict (HashMap, fromList)
import Graphics.Gloss (Picture)
import ThirdParty.GraphicsGlossGame (animation)
import Lib.Enemy.Types (EnemyType(..))
import Lib.Image (readPngOrError)
import Lib.Animation (MkAnimation)
import Lib.Spritesheet (animFrames, animFramesFlip)

type EnemyAnimationsMap = HashMap EnemyType EnemyAnimations

data EnemyAnimations = EnemyAnimations
  { moveDown  :: !MkAnimation
  , moveUp    :: !MkAnimation
  , moveRight :: !MkAnimation
  , moveLeft  :: !MkAnimation
  , dieDown   :: !MkAnimation
  , dieUp     :: !MkAnimation
  , dieRight  :: !MkAnimation
  , dieLeft   :: !MkAnimation
  }

delayBetweenFrames :: Float
delayBetweenFrames = 0.1

enemyAnimation :: [Picture] -> MkAnimation
enemyAnimation pics = animation pics delayBetweenFrames

firebugAnimations :: IO EnemyAnimations
firebugAnimations = do
  img <- readPngOrError "assets/firebug.png"
  return $ EnemyAnimations
    { moveDown  = enemyAnimation (animFrames     size (3, 0,  8, 1) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0,  8, 1) img)
    , moveRight = enemyAnimation (animFrames     size (5, 0,  8, 1) img)
    , moveLeft  = enemyAnimation (animFramesFlip size (5, 0,  8, 1) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 11, 1) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 11, 1) img)
    , dieRight  = enemyAnimation (animFrames     size (8, 0, 11, 1) img)
    , dieLeft   = enemyAnimation (animFramesFlip size (8, 0, 11, 1) img)
    }
  where size = (128, 64)

leafbugAnimations :: IO EnemyAnimations
leafbugAnimations = do
  img <- readPngOrError "assets/leafbug.png"
  return $ EnemyAnimations
    { moveDown  = enemyAnimation (animFrames     size (3, 0, 8, 1) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0, 8, 1) img)
    , moveRight = enemyAnimation (animFrames     size (5, 0, 8, 1) img)
    , moveLeft  = enemyAnimation (animFramesFlip size (5, 0, 8, 1) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 7, 1) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 7, 1) img)
    , dieRight  = enemyAnimation (animFrames     size (8, 0, 7, 1) img)
    , dieLeft   = enemyAnimation (animFramesFlip size (8, 0, 7, 1) img)
    }
  where size = (64, 64)

magmaCrabAnimations :: IO EnemyAnimations
magmaCrabAnimations = do
  img <- readPngOrError "assets/magma_crab.png"
  return $ EnemyAnimations
    { moveDown  = enemyAnimation (animFrames     size (3, 0,  8, 1) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0,  8, 1) img)
    , moveRight = enemyAnimation (animFramesFlip size (5, 0,  8, 1) img)
    , moveLeft  = enemyAnimation (animFrames     size (5, 0,  8, 1) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 10, 1) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 10, 1) img)
    , dieRight  = enemyAnimation (animFramesFlip size (8, 0, 10, 1) img)
    , dieLeft   = enemyAnimation (animFrames     size (8, 0, 10, 1) img)
    }
  where size = (64, 64)

scorpionAnimations :: IO EnemyAnimations
scorpionAnimations = do
  img <- readPngOrError "assets/scorpion.png"
  return $ EnemyAnimations
    { moveDown  = enemyAnimation (animFrames     size (3, 0, 8, 1) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0, 8, 1) img)
    , moveRight = enemyAnimation (animFramesFlip size (5, 0, 8, 1) img)
    , moveLeft  = enemyAnimation (animFrames     size (5, 0, 8, 1) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 8, 1) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 8, 1) img)
    , dieRight  = enemyAnimation (animFramesFlip size (8, 0, 8, 1) img)
    , dieLeft   = enemyAnimation (animFrames     size (8, 0, 8, 1) img)
    }
  where size = (64, 64)

clampbeetleAnimations :: IO EnemyAnimations
clampbeetleAnimations = do
  img <- readPngOrError "assets/clampbeetle.png"
  return $ EnemyAnimations
    { moveDown  = enemyAnimation (animFrames     size (3, 0,  8, 1) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0,  8, 1) img)
    , moveRight = enemyAnimation (animFramesFlip size (5, 0,  8, 1) img)
    , moveLeft  = enemyAnimation (animFrames     size (5, 0,  8, 1) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 13, 1) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 13, 1) img)
    , dieRight  = enemyAnimation (animFramesFlip size (8, 0, 13, 1) img)
    , dieLeft   = enemyAnimation (animFrames     size (8, 0, 13, 1) img)
    }
  where size = (64, 64)

firewaspAnimations :: IO EnemyAnimations
firewaspAnimations = do
  img <- readPngOrError "assets/firewasp.png"
  return $ EnemyAnimations
    { moveDown  = enemyAnimation (animFrames     size (3, 0,  8, 1) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0,  8, 1) img)
    , moveRight = enemyAnimation (animFrames     size (5, 0,  8, 1) img)
    , moveLeft  = enemyAnimation (animFramesFlip size (5, 0,  8, 1) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 12, 1) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 12, 1) img)
    , dieRight  = enemyAnimation (animFrames     size (8, 0, 12, 1) img)
    , dieLeft   = enemyAnimation (animFramesFlip size (8, 0, 12, 1) img)
    }
  where size = (96, 96)

flyingLocustAnimations :: IO EnemyAnimations
flyingLocustAnimations = do
  img <- readPngOrError "assets/flying_locust.png"
  return $ EnemyAnimations
    { moveDown  = enemyAnimation (animFrames     size (3, 0,  8, 1) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0,  8, 1) img)
    , moveRight = enemyAnimation (animFrames     size (5, 0,  8, 1) img)
    , moveLeft  = enemyAnimation (animFramesFlip size (5, 0,  8, 1) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 14, 1) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 14, 1) img)
    , dieRight  = enemyAnimation (animFrames     size (8, 0, 14, 1) img)
    , dieLeft   = enemyAnimation (animFramesFlip size (8, 0, 14, 1) img)
    }
  where size = (64, 64)

voidbutterflyAnimations :: IO EnemyAnimations
voidbutterflyAnimations = do
  img <- readPngOrError "assets/voidbutterfly.png"
  return $ EnemyAnimations
    { moveDown  = enemyAnimation (animFrames     size (3, 0,  4, 1) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0,  4, 1) img)
    , moveRight = enemyAnimation (animFrames     size (5, 0,  4, 1) img)
    , moveLeft  = enemyAnimation (animFramesFlip size (5, 0,  4, 1) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 12, 1) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 12, 1) img)
    , dieRight  = enemyAnimation (animFrames     size (8, 0, 12, 1) img)
    , dieLeft   = enemyAnimation (animFramesFlip size (8, 0, 12, 1) img)
    }
  where size = (64, 64)

allEnemyAnimations :: IO EnemyAnimationsMap
allEnemyAnimations =
  fromList <$> mapM extractSndM
    [ (Firebug, firebugAnimations)
    , (Leafbug, leafbugAnimations)
    , (MagmaCrab, magmaCrabAnimations)
    , (Scorpion, scorpionAnimations)
    , (Clampbeetle, clampbeetleAnimations)
    , (Firewasp, firewaspAnimations)
    , (FlyingLocust, flyingLocustAnimations)
    , (Voidbutterfly, voidbutterflyAnimations)
    ]
  where 
    extractSndM (a, ioB) = (a,) <$> ioB
