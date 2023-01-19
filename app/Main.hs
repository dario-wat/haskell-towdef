{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.HashMap.Strict as HM
import Lib.Window (windowSizeForInWindow, windowPositionForInWindow)
import Graphics.Gloss
import Debug.Trace (traceShowId, traceShow, trace)
import ThirdParty.GraphicsGlossGame (playInScene, picturing, noAnimation, scenes, translating)
import GameObjects.WalkingEnemy (WalkingEnemyAnimations(walkDown, walkRight, walkLeft), firebugAnimations, leafbugAnimations, magmaCrabAnimations, scorpionAnimations)
import GameObjects.Sprite (mkNonAnimatedSprite)
import qualified GameObjects.Sprite as S (Sprite(..), update, draw)
import Lib.Level.Path (genRandomPath, addPathToGrid)
import Lib.Level.Grid (emptyGrid)
import Lib.Level.MapGenerator (picturizeGrid)
import qualified Lib.Animation as A


data GameState = GameState
  { animations :: HM.HashMap String A.Animation
  }

mkGameState :: IO GameState
mkGameState = do
  fba <- firebugAnimations
  lba <- leafbugAnimations
  mca <- magmaCrabAnimations
  sca <- scorpionAnimations
  return $ GameState
    { --bug = mkNonAnimatedSprite 300 0 (-1) 0 (down fbp)
      animations = HM.fromList
        [ ("firebug", A.mkAnimation (walkRight fba) (-1))
        , ("leafbug", A.mkAnimation (walkLeft lba) (-1))
        , ("magma", A.mkAnimation (walkLeft mca) (-1))
        , ("scorpion", A.mkAnimation (walkRight sca) (-1))
        ]
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
  gs <- mkGameState

  path <- genRandomPath
  let grid = addPathToGrid emptyGrid path
  gridPic <- picturizeGrid grid
  -- putStrLn . gridArrayStr . addPathToGrid emptyGrid =<< genRandomPath
  let
    animationScenes = scenes
      [ translating (const ((-300), 100)) $ A.animating (\w -> animations w HM.! "firebug")
      , translating (const ((-300), 0)) $ A.animating (\w -> animations w HM.! "leafbug")
      , translating (const ((-300), (-100))) $ A.animating (\w -> animations w HM.! "magma")
      , translating (const ((-300), (-200))) $ A.animating (\w -> animations w HM.! "scorpion")
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
    \now _ world -> world {animations = HM.map (A.update now) $ animations world}
    ]
