{-# LANGUAGE NamedFieldPuns #-}

import Lib.Window (windowSizeForInWindow, windowPositionForInWindow)
import Graphics.Gloss
import Debug.Trace (traceShowId, traceShow, trace)
import ThirdParty.GraphicsGlossGame (playInScene, picturing, Animation, noAnimation, scenes)
import GameObjects.WalkingEnemy (WalkingEnemyAnimations(walkDown, walkRight, walkLeft), firebugAnimations, firebugPictures, WalkingEnemyPictures (down))
import Lib.Animation (drawingAnimation)
import GameObjects.Sprite (mkNonAnimatedSprite)
import qualified GameObjects.Sprite as S (Sprite(..), update, draw)
import Lib.Level.Path (genRandomPath, addPathToGrid)
import Lib.Level.Grid (emptyGrid)
import Lib.Level.MapGenerator (picturizeGrid)
import qualified Lib.Animation as A


data GameState = GameState
  { anim :: Animation
  , anim2 :: Animation
  , bug :: S.Sprite
  , an :: A.Animation
  }

mkGameState :: IO GameState
mkGameState = do
  fba <- firebugAnimations
  fbp <- firebugPictures
  return $ GameState
    { anim = noAnimation
    , anim2 = noAnimation
    , bug = mkNonAnimatedSprite 300 0 (-1) 0 (down fbp)
    , an = A.mkAnimation (walkRight fba) True
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
    animationScenes = scenes
      [ drawingAnimation 128 64 anim
      , drawingAnimation (-100) 100 anim2
      , drawingAnimation (-200) 100 anim
      -- , A.animating (-300) 100 (an gs)
      , drawingAnimation (-300) 100 (A.current . an)

      ]
  playInScene
    window 
    background 
    60
    gs 
    -- (picturing (\w -> S.draw $ bug w))
    (scenes [
      picturing $ const $ pictures [gridPic]
      , picturing (S.draw . bug)
      , animationScenes])
    (\_ _ -> id) 
    [ \_ _ (GameState anim anim2 bug an) -> GameState {anim, anim2, bug = S.update bug, an}
    , \now _ world -> world {an = A.update now (an world)}
    ]
