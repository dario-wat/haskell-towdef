{-# LANGUAGE NamedFieldPuns #-}

import Lib.Window (windowSizeForInWindow, windowPositionForInWindow)
import Graphics.Gloss
import Debug.Trace (traceShowId, traceShow, trace)
import ThirdParty.GraphicsGlossGame (playInScene, picturing, Animation, noAnimation, scenes, translating)
import GameObjects.WalkingEnemy (WalkingEnemyAnimations(walkDown, walkRight, walkLeft), firebugAnimations)
import GameObjects.Sprite (mkNonAnimatedSprite)
import qualified GameObjects.Sprite as S (Sprite(..), update, draw)
import Lib.Level.Path (genRandomPath, addPathToGrid)
import Lib.Level.Grid (emptyGrid)
import Lib.Level.MapGenerator (picturizeGrid)
import qualified Lib.Animation as A


data GameState = GameState
  { --bug :: S.Sprite
  an :: A.Animation
  }

mkGameState :: IO GameState
mkGameState = do
  fba <- firebugAnimations
  return $ GameState
    { --bug = mkNonAnimatedSprite 300 0 (-1) 0 (down fbp)
    an = A.mkAnimation (walkRight fba) (-1)
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
      [ translating (const ((-300), 100)) $ A.animating an
      ]
  playInScene
    window 
    background 
    60
    gs 
    -- (picturing (\w -> S.draw $ bug w))
    (scenes [
      picturing $ const $ pictures [gridPic]
      -- , picturing (S.draw . bug)
      , animationScenes])
    (\_ _ -> id) 
    [ -- \_ _ (GameState bug an) -> GameState {bug = S.update bug, an}
    \now _ world -> world {an = A.update now (an world)}
    ]
