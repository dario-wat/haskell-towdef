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
import Data.Maybe (isNothing)
import Lib.Animation (repeatingAnimation, drawingAnimation)
import GameObjects.Sprite (mkSprite, mkNonAnimatedSprite)
import qualified GameObjects.Sprite as S (Sprite(..), update, draw)
import Lib.Path (genRandomPath, genRandomPoints)
import Lib.Grid (emtpyGrid, gridArrayStr, gridArraysStr)
import Data.Array (Array, listArray, assocs, ixmap, elems, (//))

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
  im <- readTerrainImage
  b <- terrainObjects
  dter <- debugTerrain
  dss <- debugSpritesheet 128 64 "assets/firebug.png"
  d1 <- debugSpritesheetFramesIndexed 128 64 "assets/firebug.png" $ traceShowId $ genRowIndices 3 0 8
  fba <- firebugAnimations
  fb <- readPngOrError "assets/firebug.png"
  gs <- mkGameState
  drTer <- debugExampleTerrain
  -- putStrLn . show =<< genRandomPoints 100
  -- print gridArray
  -- rs <- genRandomPoints 20
  -- print rs
  debugPath
  -- eps <- genRandomPath
  -- print eps
  -- putStrLn . gridArraysStr . map gridPath . createAllPaths =<< genRandomPoints 5
  let
    applyBs now _ world = 
      world 
        { anim = repeatingAnimation (anim world) (walkLeft fba) now
        , anim2 = repeatingAnimation (anim2 world) (walkDown fba) now
        }
    -- pic = cropTile 5 1 im
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
    -- (picturing (\w -> S.draw $ bug w))
    (picturing $ const $ pictures [drTer, debugGrid])
    (\_ _ -> id) 
    [\_ _ (GameState anim anim2 bug) -> GameState {bug = S.update bug}]
