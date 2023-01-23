{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.HashMap.Strict as HM
import Lib.Window (windowSizeForInWindow, windowPositionForInWindow)
import Graphics.Gloss hiding (play, Path)
import qualified Graphics.Gloss as G
import ThirdParty.GraphicsGlossGame (play, picturing, scenes, drawScene)
import qualified Lib.Enemy.Animations as E
import qualified Lib.Enemy.Types as E
import qualified Lib.Enemy.Manager as E
import qualified GameObjects.Enemy as E hiding (update, draw)
import qualified GameObjects.Sprite as S
import Lib.Level.Path (genRandomPath, addPathToGrid, Path)
import Lib.Level.Grid (emptyGrid, debugGrid, gridCellOf, gridCenterOf)
import Lib.Level.MapGenerator (picturizeGrid)
import qualified Lib.Animation as A
import Debug (debugPoint)


data GameState = GameState
  { time :: Float
  , path :: Path
  , sprites :: HM.HashMap String S.Sprite
  , enemyManager :: E.EnemyManager
  }

mkGameState :: IO GameState
mkGameState = do
  allAnimations <- E.allEnemyAnimations
  fba <- E.firebugAnimations
  _lba <- E.leafbugAnimations
  _mca <- E.magmaCrabAnimations
  sca <- E.scorpionAnimations
  cba <- E.clampbeetleAnimations
  fwa <- E.firewaspAnimations
  fla <- E.flyingLocustAnimations
  vba <- E.voidbutterflyAnimations
  path <- genRandomPath
  let (sx, sy) = gridCenterOf (head path) (1, 1)
  return $ GameState
    { time = 0
    , path
    , sprites = HM.fromList
        [ --("scorpion", S.mkAnimatedSprite sx sy $ A.mkAnimation (E.moveRight sca) (-1))
         ("firebug", S.mkAnimatedSprite 200 100 $ A.mkAnimation (E.moveRight fba) (-1))
        , ("clampbeetle", S.mkAnimatedSprite 200 0 $ A.mkAnimation (E.moveRight cba) (-1))
        , ("firewasp", S.mkAnimatedSprite 200 (-100) $ A.mkAnimation (E.moveRight fwa) (-1))
        , ("flyinglocust", S.mkAnimatedSprite 200 (-200) $ A.mkAnimation (E.moveRight fla) (-1))
        , ("voidbutterfly", S.mkAnimatedSprite 200 (-300) $ A.mkAnimation (E.dieRight vba) (-1))
        ]
    , enemyManager = E.mkEnemyManager 
        allAnimations 
        [(E.Firebug, 0), (E.Leafbug, 1), (E.MagmaCrab, 1.5), (E.Scorpion, 2), (E.Clampbeetle, 8)] 
        path
    }

window :: Display
window = InWindow "Nice Window" windowSizeForInWindow windowPositionForInWindow
-- window = FullScreen  -- needs special window sizing

background :: Color
background = white

main :: IO ()
main = do
  gs <- mkGameState

  let grid = addPathToGrid emptyGrid (path gs)
  gridPic <- picturizeGrid grid

  -- let scorpion = sprites gs HM.! "scorpion"
  -- print (S.x scorpion, S.y scorpion)
  -- print (gridCellOf (S.x scorpion, S.y scorpion))
  -- print $ path gs
  -- print $ nextPoint (gridCellOf (S.x scorpion, S.y scorpion)) (tail $ path gs)
  -- print $ fst $ nextDirection (S.x scorpion, S.y scorpion) (tail $ path gs)
  let
    allScenes = scenes 
      [ picturing $ const $ pictures [gridPic]
      ]
  play
    window 
    background 
    60
    gs 
    (\world -> pictures 
      [ drawScene allScenes (time world) world
      -- , S.draw (time world) (sprites world HM.! "scorpion")
      -- , S.draw (time world) (sprites world HM.! "firebug")
      -- , S.draw (time world) (sprites world HM.! "clampbeetle")
      -- , S.draw (time world) (sprites world HM.! "firewasp")
      -- , S.draw (time world) (sprites world HM.! "flyinglocust")
      -- , S.draw (time world) (sprites world HM.! "voidbutterfly")
      -- , E.draw (time world) (enemy world)
      , E.draw (time world) (enemyManager world)
      -- , debugGrid
      -- , debugPoint (-220) 200
      -- , uncurry debugPoint $ gridCenterOf (gridCellOf (-220, 200)) (1, 1)
      ]
    )
    (\_ -> id) 
    [  
      -- It is crucial that time change is the first thing to happen 
      -- in the update function, otherwise the animations will not
      -- be updated properly.
      \dt world -> world {time = time world + dt}
     , \_ world -> world 
        { sprites = HM.map (S.update $ time world) $ sprites world
        , enemyManager = E.update (time world) (enemyManager world)
        }
    
    ]
