{-# LANGUAGE RecordWildCards #-}
module Lib where

import Hasql.Connection (Connection)
import Control.Monad.Reader
import Control.Monad.Error.Class
import Control.Monad.Logger

import Control.Exception (catch, throwIO, try, Exception)

import Toml (TomlCodec, (.=))
import qualified Toml

import qualified Hasql.Pool as Pool
import Hasql.Pool (Pool, acquire)
import Hasql.Connection (Connection, ConnectionError, release, settings)

import Control.Monad.Logger.CallStack (defaultOutput, logError)
import System.IO (stdout)
import System.Environment (getEnv, lookupEnv)
import Data.ByteString
import Data.Text
import Control.Monad (when, void)
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import Data.Int (Int32)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Bifunctor (first)

import Servant (ServerError, Handler)

import Data.Either.Combinators (whenLeft)
import Control.Monad.Except (ExceptT, runExceptT)

type LogAction = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

data Env = Env
    { envDbPool    :: !Pool
    , envLogAction :: LogAction
    , envLogLevel  :: LogLevel
    }

-- | Constaint for monadic stack that has access to a database pool.
class WithDb m where
    withPool :: (Pool -> IO a) -> m a

instance WithDb App where
    withPool f = do
        pool <- asks envDbPool
        liftIO $ f pool

newtype App a = App
    { unApp :: ReaderT Env (ExceptT ServerError IO) a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader Env
        , MonadError ServerError
        )

-- data AppError = 
--        DbError Pool.UsageError 
--     |  ServerError Text
--     |  NotFoundError Text
--     deriving (Show, Eq)

-- newtype AppException = AppException
--     { unAppException :: AppError
--     } deriving (Show)
--       deriving anyclass (Exception)

-- instance MonadError AppError App where
--     throwError :: AppError -> App a
--     throwError = liftIO . throwIO . AppException

--     catchError :: App a -> (AppError -> App a) -> App a
--     catchError action handler = App $ ReaderT $ \env -> do
--         let ioAction = runApp env action
--         ioAction `catch` \(AppException e) -> runApp env $ handler e

instance MonadLogger App where
    monadLoggerLog loc source level msg = do
        Env { envLogAction, envLogLevel } <- ask
        when (level >= envLogLevel) $
            liftIO $ envLogAction loc source level (toLogStr msg)

-- runApp :: Env -> App a -> IO (Either ServerError a)
runApp :: Env -> App a -> IO (Either ServerError a)
runApp env app = runExceptT $ runReaderT (unApp app) env

runAppAsIO :: Env -> App a -> IO a
runAppAsIO env app = runApp env app >>= either throwIO return

runAppAsHandler :: Env -> App a -> Handler a
runAppAsHandler env app = do
    res <- liftIO $ runApp env app
    liftEither res

-- runAppAsHandler :: Env -> App a -> Handler a
-- runAppAsHandler env app = do
--     res <- liftIO $ runAppLogIO env app
--     liftEither $ first toHttpError res
--     where
--         toHttpError :: AppError -> ServerError
--         toHttpError = undefined

-- runAppAsIO :: Env -> App a -> IO (Either AppError a)
-- runAppAsIO env = fmap (first unAppException) . try . runApp env

-- runAppLogIO :: Env -> App a -> IO (Either AppError a)
-- runAppLogIO env app = do
--     appRes <- runAppAsIO env app
--     whenLeft appRes $ logErrorIO env
--     return appRes

-- logErrorIO :: Env -> AppError -> IO ()
-- logErrorIO env err = void $ runAppAsIO env $ $(logErrorSH) err
            
someFunc :: IO ()
someFunc = putStrLn "someFunc"

data AppConfig = AppConfig
    { configDescription :: !Text
    , configDbPort      :: !Word16
    , configDbHost      :: !ByteString
    , configDbName      :: !ByteString
    , configDbUser      :: !ByteString
    , configDbPass      :: !ByteString
    }

configCodec :: TomlCodec AppConfig
configCodec = AppConfig
    <$> Toml.text "app.description" .= configDescription
    <*> Toml.read "db.port" .= configDbPort
    <*> Toml.byteString "db.host" .= configDbHost
    <*> Toml.byteString "db.name" .= configDbName
    <*> Toml.byteString "db.user" .= configDbUser
    <*> Toml.byteString "db.pass" .= configDbPass

loadConfig :: IO AppConfig
loadConfig = do
    configFile <- fromMaybe "config.toml" <$> lookupEnv "CONFIG_FILE"
    Toml.decodeFile configCodec configFile

initialisePool :: AppConfig -> IO Pool
initialisePool AppConfig{..} = do
    let conSettings = settings configDbHost configDbPort configDbUser configDbPass configDbName
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
 

