import Control.Concurrent (threadDelay)
import Control.Monad (forever)

import Graphics.Gloss
import Debug

data GameState = GameState
  { objectLocation :: (Float, Float)
  }

mkGameState :: GameState
mkGameState = GameState
  { objectLocation = (0, 0)
  }

render :: GameState -> Picture
render state = point x y
  where
    (x, y) = objectLocation state

update :: GameState -> GameState
update state = state { objectLocation = (x + 1, y) }
  where
    (x, y) = objectLocation state

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

-- Draw the text of the given coordinates at the location of the coordinates
coordinate :: (Int, Int) -> Picture
coordinate (x, y) = translate (fromIntegral x) (fromIntegral y) $ scale 0.5 0.5 $ text $ show (x, y)

-- main :: IO ()
-- main = do
--     putStrLn "Hello, Haskell!"
--     gameLoop $ putStrLn "Hello, game loop!"

main :: IO ()
main = play 
  window 
  background 
  60 
  mkGameState 
  render 
  (\_ -> id) 
  (\_ -> update)
