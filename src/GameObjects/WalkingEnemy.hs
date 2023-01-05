module GameObjects.WalkingEnemy
  (

  ) where

import Lib.Image (readPngOrError)
import Graphics.Gloss (Picture)
import Codec.Picture (DynamicImage)
import ThirdParty.GraphicsGlossGame (Animation, animation)
import Lib.Spritesheet (genRowIndices, framesIndexed)
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

-- firebugAnimations :: IO FirebugAnimations
-- firebugAnimations = do
--   img <- loadFirebug
--   return $ FirebugAnimations
--     { walkDown = animation (framesIndexed spriteWidth spriteHeight img $ genRowIndices 0 0 8) 0.1 0
--     , walkUp = undefined
--     , walkRight = undefined
--     , walkLeft = undefined
--     , dieDown = undefined
--     , dieUp = undefined
--     , dieRight = undefined
--     , dieLeft = undefined
--     }
  