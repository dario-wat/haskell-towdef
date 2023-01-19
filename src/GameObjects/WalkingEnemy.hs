module GameObjects.WalkingEnemy
  ( WalkingEnemyAnimations(..)
  , firebugAnimations
  ) where

-- TODO
-- Add other bugs

import Codec.Picture (DynamicImage)
import ThirdParty.GraphicsGlossGame (animation)
import Lib.Image (readPngOrError)
import Lib.Animation (MkAnimation)
import Lib.Spritesheet (animFrames, animFramesFlip)
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