import Lib.Window (windowSize, windowPosition)
import Graphics.Gloss
import Debug.Trace (traceShowId, traceShow)
import GameObjects.Terraine
import Debug (debugSpritesheet, debugTerraine, debugSpritesheetFramesIndexed)
import Lib.Spritesheet (genRowIndices, framePictures, framesIndexed, animFrames)
import ThirdParty.GraphicsGlossGame (playInScene, picturing, animating, Animation, noAnimation, animation, animationPicture)
import GameObjects.WalkingEnemy (FirebugAnimations(walkDown), firebugAnimations)
import Lib.Image (readPngOrError)
import Data.Maybe (isNothing)

data GameState = GameState
  { anim :: Animation
  }

mkGameState :: GameState
mkGameState = GameState
  { anim = noAnimation
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
  let 
    an = animation (animFrames (128, 64) (3, 0, 8) fb) 0.1
    applyBs now _ world = 
      world { anim = if isNothing $ animationPicture (anim world) now then an $ traceShowId now  else anim world}
    pic = cropTile 5 1 im
    -- im3 = crop (5*64) (1*64) 64 64 $ convertRGB8 im2
    -- pic = fromImageRGB8 im3
  playInScene
    window 
    background 
    60
    mkGameState 
    -- render 
    (animating anim blank)
    -- (picturing (\_ -> d1))
    (\_ _ -> id) 
    [applyBs]
