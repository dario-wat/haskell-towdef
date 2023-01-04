import Graphics.Gloss
import Debug.Debug (debugPoint, debugGrid)

data GameState = GameState
  { angle :: Float
  , radius :: Float
  , xOrig :: Float
  , yOrig :: Float
  }

mkGameState :: GameState
mkGameState = GameState
  { angle = 0
  , radius = 100
  , xOrig = 0
  , yOrig = 0
  }

render :: GameState -> Picture
render state = debugPoint (x + r * sin a) (y + r * cos a)
  where
    a = angle state
    x = xOrig state
    y = yOrig state
    r = radius state

update :: GameState -> GameState
update state = state { angle = a + 3 * pi / 180 }
  where
    a = angle state

windowSize :: (Int, Int)
windowSize = (1920, 1080)

windowPosition :: (Int, Int)
windowPosition = (0, 0)

window :: Display
window = InWindow "Nice Window" windowSize windowPosition
-- window = FullScreen

background :: Color
background = white

drawing :: Picture
drawing = translate 100 0 $ circle 80

main :: IO ()
main = play 
  window 
  background 
  60 
  mkGameState 
  -- render 
  (\_ -> debugGrid)
  (\_ -> id) 
  (\_ -> update)
