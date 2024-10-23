module Lib
    ( runServer
    , initDb
    , getApplication
    ) where

import Control.Monad.Logger
import Hasql.Connection (settings)
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import System.IO (stdout, stderr, hPutStrLn)
import Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, defaultSettings, runSettings)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai (Middleware, Application)
import Servant.Auth.Server (generateKey)

import Lib.App
import Lib.Config
import Lib.Server
import Lib.Db
import Lib.Core.Speller

-- | Initialize the database connection pool.
initialisePool :: AppConfig -> IO Pool
initialisePool AppConfig{..} = do
    let conSettings = settings dbHost (fromIntegral dbPort) dbUser dbPass dbName
        poolSize = 10
        poolAcquisitionTimeout = 10
        poolTimeout = 60 -- one minute
        poolLifeTime = 600 -- ten minutes
    Pool.acquire poolSize poolAcquisitionTimeout poolLifeTime poolTimeout conSettings

-- | Create the application environment.
mkEnv :: AppConfig -> IO Env
mkEnv config = do
    dbPool <- initialisePool config
    jwtKey <- generateKey
    let logAction = defaultOutput stdout
    let spellerAction = 
          if spellerEnabled config
              then externalSpeller @AppM (spellerUri config)
              else mockSpeller @AppM
    return Env
        { envDbPool = dbPool
        , envLogAction = logAction
        , envLogLevel = LevelDebug
        , envSpellerAction = spellerAction
        , envJWTKey = jwtKey
        , envConfig = config
        }

-- | Create the logging middleware.
mkLoggers :: AppConfig -> IO Middleware
mkLoggers _ = 
    let loggerSettings = defaultRequestLoggerSettings {
          outputFormat = Apache FromHeader,      -- Use Apache log format
          destination = Handle stderr            -- Log to stderr 
        }
    in mkRequestLogger loggerSettings

-- | Middleware to allow CSRF headers.
allowCsrf :: Middleware
allowCsrf = addHeaders [("Access-Control-Allow-Headers", "x-csrf-token,authorization")]

-- | Middleware to enable CORS.
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

-- | Used for testing. Get the WAI application.
getApplication :: IO Application
getApplication = do
    config <- loadConfig
    env <- mkEnv config
    return $ application env

-- | Run the server with the given configuration.
runServer :: IO ()
runServer = do
    config <- loadConfig
    env <- mkEnv config
    loggers <- mkLoggers config
    let warpSettings =
            setPort (appPort config) $
            setBeforeMainLoop (hPutStrLn stderr ("Starting server on port " ++ show (appPort config) ++ "..."))
            defaultSettings
    runSettings warpSettings $ loggers . allowCsrf . corsified $ application env

-- | Initialize the database with the given configuration, and seed it.
initDb :: IO ()
initDb = do
    config <- loadConfig
    env <- mkEnv config
    hPutStrLn stderr "Initializing and seeding database..."
    runAppAsIO env prepareSeededDb
