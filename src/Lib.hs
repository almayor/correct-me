module Lib
    ( runServer
    , initDb
    ) where

import Control.Monad.Logger
import Hasql.Connection (settings)
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import System.IO (stdout)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
import Network.Wai (Middleware)

import Lib.App
import Lib.Config
import Lib.Server
import Lib.Db
import Lib.Core.Speller

initialisePool :: AppConfig -> IO Pool
initialisePool AppConfig{..} = do
    let conSettings = settings configDbHost (fromIntegral configDbPort) configDbUser configDbPass configDbName
        poolSize = 10
        poolAcquisitionTimeout = 10
        poolTimeout = 60 -- one minute
        poolLifeTime = 600 -- ten minutes
    Pool.acquire poolSize poolAcquisitionTimeout poolLifeTime poolTimeout conSettings

mkEnv :: AppConfig -> IO Env
mkEnv config@Config(configSpellerUri, configSpellerEnabled) = do
    dbPool <- initialisePool config
    let logAction = defaultOutput stdout
    let spellerAction = 
          if configSpellerEnabled
              then externalSpeller @App configSpellerUri
              else mockSpeller @App
    return Env
        { envDbPool = dbPool
        , envLogAction = logAction
        , envLogLevel = LevelDebug
        , envSpellerAction = spellerAction
        }

mkLoggers :: AppConfig -> IO Middleware
mkLoggers _ = 
    let stdoutLoggerSettings = defaultRequestLoggerSettings {
          outputFormat = Apache FromHeader,      -- Use Apache log format
          destination = Handle stdout            -- Log to stdout 
        }
    in mkRequestLogger stdoutLoggerSettings

runServer :: IO ()
runServer = do
    config <- loadConfig
    env <- mkEnv config
    loggers <- mkLoggers config
    putStrLn $ "Starting server on port " ++ show (configAppPort config)
    run (configAppPort config) $ loggers $ application env

initDb :: IO ()
initDb = do
    config <- loadConfig
    env <- mkEnv config
    putStrLn "Initialising and seeding database"
    runAppAsIO env prepareSeededDb
    
 

