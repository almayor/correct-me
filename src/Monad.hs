module Monad where

import Control.Monad.Reader
import Control.Monad.Error.Class
import Control.Monad.Logger
import Hasql.Pool (Pool)

import Control.Monad (when)


import Servant (ServerError, Handler)

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Exception (throwIO)

type LogAction = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

data Env = Env
    { envDbPool    :: !Pool
    , envLogAction :: LogAction
    , envLogLevel  :: LogLevel
    }

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

instance MonadLogger App where
    monadLoggerLog loc source level msg = do
        Env { envLogAction, envLogLevel } <- ask
        when (level >= envLogLevel) $
            liftIO $ envLogAction loc source level (toLogStr msg)

-- | Constaint for monadic stack that has access to a database pool.
class WithDb m where
    withPool :: (Pool -> IO a) -> m a

instance WithDb App where
    withPool f = do
        pool <- asks envDbPool
        liftIO $ f pool

runApp :: Env -> App a -> IO (Either ServerError a)
runApp env app = runExceptT $ runReaderT (unApp app) env

runAppAsIO :: Env -> App a -> IO a
runAppAsIO env app = runApp env app >>= either throwIO return

runAppAsHandler :: Env -> App a -> Handler a
runAppAsHandler env app = do
    res <- liftIO $ runApp env app
    liftEither res
