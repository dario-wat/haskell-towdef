{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.HashMap.Strict as HM
import Lib.Window (windowSizeForInWindow, windowPositionForInWindow)
import Graphics.Gloss hiding (play)
import ThirdParty.GraphicsGlossGame (play, picturing, scenes, drawScene)
import qualified GameObjects.Enemy as E
import GameObjects.Sprite (mkAnimatedSprite)
import qualified GameObjects.Sprite as S
import Lib.Level.Path (genRandomPath, addPathToGrid)
import Lib.Level.Grid (emptyGrid)
import Lib.Level.MapGenerator (picturizeGrid)
import qualified Lib.Animation as A


data GameState = GameState
  { time :: Float
  , sprites :: HM.HashMap String S.Sprite
  }

mkGameState :: IO GameState
mkGameState = do
  fba <- E.firebugAnimations
  _lba <- E.leafbugAnimations
  _mca <- E.magmaCrabAnimations
  sca <- E.scorpionAnimations
  return $ GameState
    { time = 0
    , sprites = HM.fromList
        [ ("scorpion", mkAnimatedSprite 300 100 $ A.mkAnimation (E.moveRight sca) (-1))
        , ("firebug", mkAnimatedSprite 200 100 $ A.mkAnimation (E.moveRight fba) (-1))
        ]
    }

window :: Display
window = InWindow "Nice Window" windowSizeForInWindow windowPositionForInWindow
-- window = FullScreen  -- needs special window sizing

background :: Color
background = white

main :: IO ()
main = do
  gs <- mkGameState

  path <- genRandomPath
  let grid = addPathToGrid emptyGrid path
  gridPic <- picturizeGrid grid
  -- putStrLn . gridArrayStr . addPathToGrid emptyGrid =<< genRandomPath
  let
    allScenes = scenes 
      [ picturing $ const $ pictures [gridPic]
      ]
  play
    window 
    background 
    60
    gs 
    -- (picturing (\w -> S.draw $ bug w))
    (\world -> pictures 
      [ drawScene allScenes (time world) world
      , S.draw (time world) (sprites world HM.! "scorpion")
      , S.draw (time world) (sprites world HM.! "firebug")
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
        
        }
    
    ]
