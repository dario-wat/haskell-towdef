import Lib.Window (windowSize, windowPosition)
import Graphics.Gloss
import Debug.Trace (traceShowId, traceShow)
import GameObjects.Terraine
import Debug (debugSpritesheet, debugTerraine, debugSpritesheetFramesIndexed)
import Lib.Spritesheet (genRowIndices, framePictures, framesIndexed, animFrames)
import ThirdParty.GraphicsGlossGame (playInScene, picturing, animating, Animation, noAnimation, animation, animationPicture, translating, scenes)
import GameObjects.WalkingEnemy (FirebugAnimations(walkDown, walkLeft), firebugAnimations)
import Lib.Image (readPngOrError)
import Data.Maybe (isNothing)
import Lib.Animation (repeatingAnimation, drawingAnimation)
import GameObjects.Sprite (Sprite, mkSprite)

data GameState = GameState
  { anim :: Animation
  , anim2 :: Animation
  , bug :: Sprite
  }

mkGameState :: IO GameState
mkGameState = do
  fba <- firebugAnimations
  return $ GameState
    { anim = noAnimation
    , anim2 = noAnimation
    , bug = mkSprite 300 0 blank (walkDown fba)
    }

window :: Display
window = InWindow "Nice Window" windowSize windowPosition
-- window = FullScreen  -- needs special window sizing

background :: Color
background = white

-- applyBs :: Float -> Float -> GameState -> GameState
-- applyBs now _ world = world { anim = noAnimation }

main :: IO ()
main = do
  im <- readTerraineImage
  b <- terraineObjects
  dter <- debugTerraine
  dss <- debugSpritesheet 128 64 "assets/firebug.png"
  d1 <- debugSpritesheetFramesIndexed 128 64 "assets/firebug.png" $ traceShowId $ genRowIndices 3 0 8
  fba <- firebugAnimations
  fb <- readPngOrError "assets/firebug.png"
  gs <- mkGameState
  let 
    applyBs now _ world = 
      world 
        { anim = repeatingAnimation (anim world) (walkLeft fba) now
        , anim2 = repeatingAnimation (anim2 world) (walkDown fba) now
        }
    pic = cropTile 5 1 im
    -- im3 = crop (5*64) (1*64) 64 64 $ convertRGB8 im2
    -- pic = fromImageRGB8 im3
    animationScenes = scenes
      [ drawingAnimation 128 64 anim
      , drawingAnimation (-100) 100 anim2
      , drawingAnimation (-200) 100 anim
      ]
  playInScene
    window 
    background 
    60
    gs 
    -- render 
    animationScenes
    -- (picturing (\_ -> d1))
    (\_ _ -> id) 
    [applyBs]
