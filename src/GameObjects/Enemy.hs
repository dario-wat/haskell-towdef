module GameObjects.Enemy
  ( EnemyAnimations(..)
  , firebugAnimations
  , leafbugAnimations
  , magmaCrabAnimations
  , scorpionAnimations
  ) where

import ThirdParty.GraphicsGlossGame (animation)
import Lib.Image (readPngOrError)
import Lib.Animation (MkAnimation)
import Lib.Spritesheet (animFrames, animFramesFlip)

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

firebugAnimations :: IO EnemyAnimations
firebugAnimations = do
  img <- readPngOrError "assets/firebug.png"
  return $ EnemyAnimations
    { moveDown  = animation (animFrames     size (3, 0,  8) img) 0.1
    , moveUp    = animation (animFrames     size (4, 0,  8) img) 0.1
    , moveRight = animation (animFrames     size (5, 0,  8) img) 0.1
    , moveLeft  = animation (animFramesFlip size (5, 0,  8) img) 0.1
    , dieDown   = animation (animFrames     size (6, 0, 11) img) 0.1
    , dieUp     = animation (animFrames     size (7, 0, 11) img) 0.1
    , dieRight  = animation (animFrames     size (8, 0, 11) img) 0.1
    , dieLeft   = animation (animFramesFlip size (8, 0, 11) img) 0.1
    }
  where size = (128, 64)

leafbugAnimations :: IO EnemyAnimations
leafbugAnimations = do
  img <- readPngOrError "assets/leafbug.png"
  return $ EnemyAnimations
    { moveDown  = animation (animFrames     size (3, 0, 8) img) 0.1
    , moveUp    = animation (animFrames     size (4, 0, 8) img) 0.1
    , moveRight = animation (animFrames     size (5, 0, 8) img) 0.1
    , moveLeft  = animation (animFramesFlip size (5, 0, 8) img) 0.1
    , dieDown   = animation (animFrames     size (6, 0, 7) img) 0.1
    , dieUp     = animation (animFrames     size (7, 0, 7) img) 0.1
    , dieRight  = animation (animFrames     size (8, 0, 7) img) 0.1
    , dieLeft   = animation (animFramesFlip size (8, 0, 7) img) 0.1
    }
  where size = (64, 64)

magmaCrabAnimations :: IO EnemyAnimations
magmaCrabAnimations = do
  img <- readPngOrError "assets/magma_crab.png"
  return $ EnemyAnimations
    { moveDown  = animation (animFrames     size (3, 0,  8) img) 0.1
    , moveUp    = animation (animFrames     size (4, 0,  8) img) 0.1
    , moveRight = animation (animFramesFlip size (5, 0,  8) img) 0.1
    , moveLeft  = animation (animFrames     size (5, 0,  8) img) 0.1
    , dieDown   = animation (animFrames     size (6, 0, 10) img) 0.1
    , dieUp     = animation (animFrames     size (7, 0, 10) img) 0.1
    , dieRight  = animation (animFramesFlip size (8, 0, 10) img) 0.1
    , dieLeft   = animation (animFrames     size (8, 0, 10) img) 0.1
    }
  where size = (64, 64)

scorpionAnimations :: IO EnemyAnimations
scorpionAnimations = do
  img <- readPngOrError "assets/scorpion.png"
  return $ EnemyAnimations
    { moveDown  = animation (animFrames     size (3, 0, 8) img) 0.1
    , moveUp    = animation (animFrames     size (4, 0, 8) img) 0.1
    , moveRight = animation (animFramesFlip size (5, 0, 8) img) 0.1
    , moveLeft  = animation (animFrames     size (5, 0, 8) img) 0.1
    , dieDown   = animation (animFrames     size (6, 0, 8) img) 0.1
    , dieUp     = animation (animFrames     size (7, 0, 8) img) 0.1
    , dieRight  = animation (animFramesFlip size (8, 0, 8) img) 0.1
    , dieLeft   = animation (animFrames     size (8, 0, 8) img) 0.1
    }
  where size = (64, 64)