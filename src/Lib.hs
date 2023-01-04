module Lib
    ( someFunc
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Call the function every 1 second
loopFn :: IO () -> IO ()
loopFn fn = forever $ do
    fn
    threadDelay 1000000

