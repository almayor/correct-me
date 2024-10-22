module Main (main) where

import Control.Concurrent.Async (async)
import Lib

main :: IO ()
main = do
    initDb
    _ <- async runServer
    runSwagger
