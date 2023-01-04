module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

-- Runs at 60 frames per second
gameLoop :: IO () -> IO ()
gameLoop fn = forever $ do
    fn
    threadDelay 16666

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    gameLoop $ putStrLn "Hello, game loop!"
