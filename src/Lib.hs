module Lib
    ( runServer
    , initDb
    , runSwagger
    ) where

import Control.Monad.Logger
import Hasql.Connection (settings)
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import System.IO (stdout, stderr, hPutStrLn)
import Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, defaultSettings, runSettings)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors (simpleCors, CorsResourcePolicy (..), cors)
import Network.Wai (Middleware)
import Servant.Auth.Server (generateKey)

import Lib.App
import Lib.Config
import Lib.Server
import Lib.Db
import Lib.Core.Speller
import Lib.Swagger
import Network.Wai.Middleware.AddHeaders (addHeaders)

initialisePool :: AppConfig -> IO Pool
initialisePool AppConfig{..} = do
    let conSettings = settings dbHost (fromIntegral dbPort) dbUser dbPass dbName
        poolSize = 10
        poolAcquisitionTimeout = 10
        poolTimeout = 60 -- one minute
        poolLifeTime = 600 -- ten minutes
    Pool.acquire poolSize poolAcquisitionTimeout poolLifeTime poolTimeout conSettings

mkEnv :: AppConfig -> IO Env
mkEnv config = do
    dbPool <- initialisePool config
    jwtKey <- generateKey
    let logAction = defaultOutput stdout
    let spellerAction = 
          if spellerEnabled config
              then externalSpeller @App (spellerUri config)
              else mockSpeller @App
    return Env
        { envDbPool = dbPool
        , envLogAction = logAction
        , envLogLevel = LevelDebug
        , envSpellerAction = spellerAction
        , envJWTKey = jwtKey
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
    let warpSettings =
            setPort (appPort config) $
            setBeforeMainLoop (hPutStrLn stderr ("Starting server on port " ++ show (appPort config)))
            defaultSettings
    runSettings warpSettings $ loggers . allowCsrf . corsified $ application env
    where
        --  CORS required by Swagger UI
        allowCsrf :: Middleware
        allowCsrf = addHeaders [("Access-Control-Allow-Headers", "x-csrf-token,authorization")]
        --  CORS required by Swagger UI
        corsified :: Middleware
        corsified = cors (const $ Just CorsResourcePolicy {
              corsOrigins        = Nothing
            , corsMethods        = ["OPTIONS", "GET", "PUT", "POST"]
            , corsRequestHeaders = ["Authorization", "Content-Type"]
            , corsExposedHeaders = Nothing
            , corsMaxAge         = Nothing
            , corsVaryOrigin     = False
            , corsRequireOrigin  = False
            , corsIgnoreFailures = False
            })

initDb :: IO ()
initDb = do
    config <- loadConfig
    env <- mkEnv config
    putStrLn "Initialising and seeding database"
    runAppAsIO env prepareSeededDb

runSwagger :: IO ()
runSwagger = do
    config <- loadConfig
    let warpSettings =
            setPort (swaggerPort config) $
            setBeforeMainLoop (hPutStrLn stderr ("Starting swagger on port " ++ show (swaggerPort config)))
            defaultSettings
    runSettings warpSettings $ applicationSwagger config
