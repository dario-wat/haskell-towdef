{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.HashMap.Strict as HM
import Lib.Window (windowSizeForInWindow, windowPositionForInWindow)
import Graphics.Gloss
import Debug.Trace (traceShowId, traceShow, trace)
import ThirdParty.GraphicsGlossGame (playInScene, picturing, noAnimation, scenes, translating)
import GameObjects.Enemy (EnemyAnimations(moveDown, moveRight, moveLeft), firebugAnimations, leafbugAnimations, magmaCrabAnimations, scorpionAnimations)
import GameObjects.Sprite (mkNonAnimatedSprite, mkAnimatedSprite)
import qualified GameObjects.Sprite as S
import Lib.Level.Path (genRandomPath, addPathToGrid)
import Lib.Level.Grid (emptyGrid)
import Lib.Level.MapGenerator (picturizeGrid)
import qualified Lib.Animation as A


data GameState = GameState
  { animations :: HM.HashMap String A.Animation
  , sprites :: HM.HashMap String S.Sprite
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
        [ ("firebug", A.mkAnimation (moveRight fba) (-1))
        , ("leafbug", A.mkAnimation (moveLeft lba) (-1))
        , ("magma", A.mkAnimation (moveLeft mca) (-1))
        -- , ("scorpion", A.mkAnimation (moveRight sca) (-1))
        ]
    , sprites = HM.fromList
        [ ("scorpion", mkAnimatedSprite 300 100 $ A.mkAnimation (moveRight sca) (-1))

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
      -- , translating (const ((-300), (-200))) $ A.animating (\w -> animations w HM.! "scorpion")
      , S.draw $ sprites gs HM.! "scorpion"
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
      \now _ world -> world 
        { animations = HM.map (A.update now) $ animations world
        , sprites = HM.map (S.update now) $ sprites world}
    ]
