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
    { moveDown  = enemyAnimation (animFrames     size (3, 0,  8) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0,  8) img)
    , moveRight = enemyAnimation (animFrames     size (5, 0,  8) img)
    , moveLeft  = enemyAnimation (animFramesFlip size (5, 0,  8) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 11) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 11) img)
    , dieRight  = enemyAnimation (animFrames     size (8, 0, 11) img)
    , dieLeft   = enemyAnimation (animFramesFlip size (8, 0, 11) img)
    }
  where size = (128, 64)

leafbugAnimations :: IO EnemyAnimations
leafbugAnimations = do
  img <- readPngOrError "assets/leafbug.png"
  return $ EnemyAnimations
    { moveDown  = enemyAnimation (animFrames     size (3, 0, 8) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0, 8) img)
    , moveRight = enemyAnimation (animFrames     size (5, 0, 8) img)
    , moveLeft  = enemyAnimation (animFramesFlip size (5, 0, 8) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 7) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 7) img)
    , dieRight  = enemyAnimation (animFrames     size (8, 0, 7) img)
    , dieLeft   = enemyAnimation (animFramesFlip size (8, 0, 7) img)
    }
  where size = (64, 64)

magmaCrabAnimations :: IO EnemyAnimations
magmaCrabAnimations = do
  img <- readPngOrError "assets/magma_crab.png"
  return $ EnemyAnimations
    { moveDown  = enemyAnimation (animFrames     size (3, 0,  8) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0,  8) img)
    , moveRight = enemyAnimation (animFramesFlip size (5, 0,  8) img)
    , moveLeft  = enemyAnimation (animFrames     size (5, 0,  8) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 10) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 10) img)
    , dieRight  = enemyAnimation (animFramesFlip size (8, 0, 10) img)
    , dieLeft   = enemyAnimation (animFrames     size (8, 0, 10) img)
    }
  where size = (64, 64)

scorpionAnimations :: IO EnemyAnimations
scorpionAnimations = do
  img <- readPngOrError "assets/scorpion.png"
  return $ EnemyAnimations
    { moveDown  = enemyAnimation (animFrames     size (3, 0, 8) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0, 8) img)
    , moveRight = enemyAnimation (animFramesFlip size (5, 0, 8) img)
    , moveLeft  = enemyAnimation (animFrames     size (5, 0, 8) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 8) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 8) img)
    , dieRight  = enemyAnimation (animFramesFlip size (8, 0, 8) img)
    , dieLeft   = enemyAnimation (animFrames     size (8, 0, 8) img)
    }
  where size = (64, 64)

clampbeetleAnimations :: IO EnemyAnimations
clampbeetleAnimations = do
  img <- readPngOrError "assets/clampbeetle.png"
  return $ EnemyAnimations
    { moveDown  = enemyAnimation (animFrames     size (3, 0,  8) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0,  8) img)
    , moveRight = enemyAnimation (animFramesFlip size (5, 0,  8) img)
    , moveLeft  = enemyAnimation (animFrames     size (5, 0,  8) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 13) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 13) img)
    , dieRight  = enemyAnimation (animFramesFlip size (8, 0, 13) img)
    , dieLeft   = enemyAnimation (animFrames     size (8, 0, 13) img)
    }
  where size = (64, 64)

firewaspAnimations :: IO EnemyAnimations
firewaspAnimations = do
  img <- readPngOrError "assets/firewasp.png"
  return $ EnemyAnimations
    { moveDown  = enemyAnimation (animFrames     size (3, 0,  8) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0,  8) img)
    , moveRight = enemyAnimation (animFrames     size (5, 0,  8) img)
    , moveLeft  = enemyAnimation (animFramesFlip size (5, 0,  8) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 12) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 12) img)
    , dieRight  = enemyAnimation (animFrames     size (8, 0, 12) img)
    , dieLeft   = enemyAnimation (animFramesFlip size (8, 0, 12) img)
    }
  where size = (96, 96)

flyingLocustAnimations :: IO EnemyAnimations
flyingLocustAnimations = do
  img <- readPngOrError "assets/flying_locust.png"
  return $ EnemyAnimations
    { moveDown  = enemyAnimation (animFrames     size (3, 0,  8) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0,  8) img)
    , moveRight = enemyAnimation (animFrames     size (5, 0,  8) img)
    , moveLeft  = enemyAnimation (animFramesFlip size (5, 0,  8) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 14) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 14) img)
    , dieRight  = enemyAnimation (animFrames     size (8, 0, 14) img)
    , dieLeft   = enemyAnimation (animFramesFlip size (8, 0, 14) img)
    }
  where size = (64, 64)

voidbutterflyAnimations :: IO EnemyAnimations
voidbutterflyAnimations = do
  img <- readPngOrError "assets/voidbutterfly.png"
  return $ EnemyAnimations
    { moveDown  = enemyAnimation (animFrames     size (3, 0,  4) img)
    , moveUp    = enemyAnimation (animFrames     size (4, 0,  4) img)
    , moveRight = enemyAnimation (animFrames     size (5, 0,  4) img)
    , moveLeft  = enemyAnimation (animFramesFlip size (5, 0,  4) img)
    , dieDown   = enemyAnimation (animFrames     size (6, 0, 12) img)
    , dieUp     = enemyAnimation (animFrames     size (7, 0, 12) img)
    , dieRight  = enemyAnimation (animFrames     size (8, 0, 12) img)
    , dieLeft   = enemyAnimation (animFramesFlip size (8, 0, 12) img)
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
