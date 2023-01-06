module GameObjects.WalkingEnemy
  ( firebugAnimations
  , FirebugAnimations(..)
  ) where

import Lib.Image (readPngOrError)
import Graphics.Gloss (Picture)
import Codec.Picture (DynamicImage)
import ThirdParty.GraphicsGlossGame (Animation, animation)
import Lib.Spritesheet (genRowIndices, framesIndexed, framePictures, animFrames)
import Const (spriteHeight, spriteWidth)

loadFirebug :: IO DynamicImage
loadFirebug = readPngOrError "assets/firebug.png"

data FirebugAnimations = FirebugAnimations
  { walkDown :: Animation
  , walkUp :: Animation
  , walkRight :: Animation
  , walkLeft :: Animation
  , dieDown :: Animation
  , dieUp :: Animation
  , dieRight :: Animation
  , dieLeft :: Animation
  }

firebugAnimations :: IO FirebugAnimations
firebugAnimations = do
  img <- loadFirebug
  return $ FirebugAnimations
    { walkDown = animation (animFrames (128, 64) (3, 0, 8) img) 0.1 0
    , walkUp = undefined
    , walkRight = undefined
    , walkLeft = undefined
    , dieDown = undefined
    , dieUp = undefined
    , dieRight = undefined
    , dieLeft = undefined
    }
  