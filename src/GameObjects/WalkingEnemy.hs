module GameObjects.WalkingEnemy
  ( firebugAnimations
  , FirebugAnimations(..)
  ) where

import Lib.Image (readPngOrError)
import Lib.Animation (AnimationBuilder)
import Codec.Picture (DynamicImage)
import ThirdParty.GraphicsGlossGame (animation)
import Lib.Spritesheet (animFrames, animFramesFlip)

loadFirebug :: IO DynamicImage
loadFirebug = readPngOrError "assets/firebug.png"

data FirebugAnimations = FirebugAnimations
  { walkDown  :: AnimationBuilder
  , walkUp    :: AnimationBuilder
  , walkRight :: AnimationBuilder
  , walkLeft  :: AnimationBuilder
  , dieDown   :: AnimationBuilder
  , dieUp     :: AnimationBuilder
  , dieRight  :: AnimationBuilder
  , dieLeft   :: AnimationBuilder
  }

firebugAnimations :: IO FirebugAnimations
firebugAnimations = do
  img <- loadFirebug
  return $ FirebugAnimations
    { walkDown  = animation (animFrames     (128, 64) (3, 0,  8) img) 0.1
    , walkUp    = animation (animFrames     (128, 64) (4, 0,  8) img) 0.1
    , walkRight = animation (animFrames     (128, 64) (5, 0,  8) img) 0.1
    , walkLeft  = animation (animFramesFlip (128, 64) (5, 0,  8) img) 0.1
    , dieDown   = animation (animFrames     (128, 64) (6, 0, 11) img) 0.1
    , dieUp     = animation (animFrames     (128, 64) (7, 0, 11) img) 0.1
    , dieRight  = animation (animFrames     (128, 64) (8, 0, 11) img) 0.1
    , dieLeft   = animation (animFramesFlip (128, 64) (8, 0, 11) img) 0.1
    }
  