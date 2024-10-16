module Main (main) where

import Lib

main :: IO ()
main = do
    print "Hello world!"
    initDb
    runServer
