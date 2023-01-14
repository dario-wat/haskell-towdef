{-# LANGUAGE NamedFieldPuns #-}

import Lib.Window (windowSize, windowSizeForInWindow, windowPositionForInWindow)
import Graphics.Gloss
import Debug.Trace (traceShowId, traceShow, trace)
import GameObjects.Terrain
import Debug
import Lib.Spritesheet (genRowIndices, framePictures, framesIndexed, animFrames)
import ThirdParty.GraphicsGlossGame (playInScene, picturing, animating, Animation, noAnimation, animation, animationPicture, translating, scenes)
import GameObjects.WalkingEnemy (WalkingEnemyAnimations(walkDown, walkLeft), firebugAnimations, firebugPictures, WalkingEnemyPictures (down))
import Lib.Image (readPngOrError)
import Data.Maybe (isNothing, fromJust)
import Lib.Animation (repeatingAnimation, drawingAnimation)
import GameObjects.Sprite (mkSprite, mkNonAnimatedSprite)
import qualified GameObjects.Sprite as S (Sprite(..), update, draw)
import Lib.Level.Path (genRandomPath, addPathToGrid)
import Data.Array (Array, listArray, assocs, ixmap, elems, (//))
import Lib.Level.Grid (emptyGrid)
import Lib.Level.MapGenerator (picturizeGrid)
import Codec.Picture (convertRGBA8, Pixel (pixelAt), Image (imageData, imageWidth, imageHeight), PixelRGBA8 (PixelRGBA8))
import ThirdParty.GraphicsGlossJuicy (fromDynamicImage, fromImageRGBA8)
import Codec.Picture.Extra (crop)


data GameState = GameState
  { anim :: Animation
  , anim2 :: Animation
  , bug :: S.Sprite
  }

mkGameState :: IO GameState
mkGameState = do
  fba <- firebugAnimations
  fbp <- firebugPictures
  return $ GameState
    { anim = noAnimation
    , anim2 = noAnimation
    , bug = mkNonAnimatedSprite 300 0 (-1) 0 (down fbp)
    }

window :: Display
window = InWindow "Nice Window" windowSizeForInWindow windowPositionForInWindow
-- window = FullScreen  -- needs special window sizing

background :: Color
background = white

-- applyBs :: Float -> Float -> GameState -> GameState
-- applyBs now _ world = world { anim = noAnimation }

main :: IO ()
main = do
  fba <- firebugAnimations
  gs <- mkGameState

  path <- genRandomPath
  let grid = addPathToGrid emptyGrid path
  gridPic <- picturizeGrid grid
  -- putStrLn . gridArrayStr . addPathToGrid emptyGrid =<< genRandomPath
  let
    applyBs now _ world = 
      world 
        { anim = repeatingAnimation (anim world) (walkLeft fba) now
        , anim2 = repeatingAnimation (anim2 world) (walkDown fba) now
        }
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
    -- (picturing (\w -> S.draw $ bug w))
    (picturing $ const $ pictures [gridPic])
    (\_ _ -> id) 
    [\_ _ (GameState anim anim2 bug) -> GameState {bug = S.update bug}]
