module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Lib

main :: IO ()
main = do
    initDb
    _ <- async runServer
    threadDelay 50000  -- wait for 50 ms
    runSwagger
