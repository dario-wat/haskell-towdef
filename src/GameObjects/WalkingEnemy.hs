module GameObjects.WalkingEnemy
  ( firebugAnimations
  , firebugPictures
  , WalkingEnemyPictures(..)
  , WalkingEnemyAnimations(..)
  ) where

-- TODO
-- 1. data type should be generic for all walking enemies

import Lib.Image (readPngOrError, cropFrame, cropFrameAndFlip)
import Lib.Animation (MkAnimation)
import Codec.Picture (DynamicImage)
import ThirdParty.GraphicsGlossGame (animation)
import Lib.Spritesheet (animFrames, animFramesFlip)
import Graphics.Gloss (Picture)
import Const (enemySpriteWidth, enemySpriteHeight)

loadFirebug :: IO DynamicImage
loadFirebug = readPngOrError "assets/firebug.png"

data WalkingEnemyAnimations = WalkingEnemyAnimations
  { walkDown  :: MkAnimation
  , walkUp    :: MkAnimation
  , walkRight :: MkAnimation
  , walkLeft  :: MkAnimation
  , dieDown   :: MkAnimation
  , dieUp     :: MkAnimation
  , dieRight  :: MkAnimation
  , dieLeft   :: MkAnimation
  }

data WalkingEnemyPictures = WalkingEnemyPictures
  { down  :: Picture
  , up    :: Picture
  , right :: Picture
  , left  :: Picture
  }

firebugAnimations :: IO WalkingEnemyAnimations
firebugAnimations = do
  img <- loadFirebug
  return $ WalkingEnemyAnimations
    { walkDown  = animation (animFrames     size (3, 0,  8) img) 0.1
    , walkUp    = animation (animFrames     size (4, 0,  8) img) 0.1
    , walkRight = animation (animFrames     size (5, 0,  8) img) 0.1
    , walkLeft  = animation (animFramesFlip size (5, 0,  8) img) 0.1
    , dieDown   = animation (animFrames     size (6, 0, 11) img) 0.1
    , dieUp     = animation (animFrames     size (7, 0, 11) img) 0.1
    , dieRight  = animation (animFrames     size (8, 0, 11) img) 0.1
    , dieLeft   = animation (animFramesFlip size (8, 0, 11) img) 0.1
    }
  where size = (enemySpriteWidth, enemySpriteHeight)
  
firebugPictures :: IO WalkingEnemyPictures
firebugPictures = do
  img <- loadFirebug
  return $ WalkingEnemyPictures
    { down  = cropFrame        3 0 w h img
    , up    = cropFrame        4 0 w h img
    , right = cropFrame        5 0 w h img
    , left  = cropFrameAndFlip 5 0 w h img
    }
  where (w, h) = (enemySpriteWidth, enemySpriteHeight)