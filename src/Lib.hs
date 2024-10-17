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


import Lib.App
import Lib.Config
import Lib.Server
import Lib.Db
import Network.Wai (Middleware)

initialisePool :: AppConfig -> IO Pool
initialisePool AppConfig{..} = do
    let conSettings = settings configDbHost (fromIntegral configDbPort) configDbUser configDbPass configDbName
        poolSize = 10
        poolAcquisitionTimeout = 10
        poolTimeout = 60 -- one minute
        poolLifeTime = 600 -- ten minutes
    Pool.acquire poolSize poolAcquisitionTimeout poolLifeTime poolTimeout conSettings

mkEnv :: AppConfig -> IO Env
mkEnv config = do
    dbPool <- initialisePool config
    let logAction = defaultOutput stdout
    return Env
        { envDbPool = dbPool
        , envLogAction = logAction
        , envLogLevel = LevelDebug
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
    run (configAppPort config) $ loggers $ application env

initDb :: IO ()
initDb = do
    config <- loadConfig
    env <- mkEnv config
    runAppAsIO env prepareSeededDb
    
 

