module Lib.App.Monad
    ( LogAction
    , Env(..)
    , AppM(..)
    , runApp
    , runAppAsIO
    , runAppAsHandler
    , WithDb(..)
    , HasSpeller(..)
    ) where

import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Error.Class
import Control.Monad.Logger
import Control.Exception (throwIO)
import Data.Text (Text)
import Data.Bifunctor (first)

import Hasql.Pool (Pool)
import Servant (Handler)
import Crypto.JOSE.JWK (JWK)

import Lib.Types
import Lib.App.Error
import Lib.Config (AppConfig)

type LogAction = Loc -> LogSource -> LogLevel -> LogStr -> IO ()
type SpellerAction = Text -> AppM SpellCheck

data Env = Env
    { envDbPool         :: !Pool
    , envLogAction      :: !LogAction
    , envLogLevel       :: !LogLevel
    , envSpellerAction  :: SpellerAction
    , envJWTKey         :: !JWK
    , envConfig         :: !AppConfig
    }

newtype AppM a = AppM
    { unAppM :: ReaderT Env (ExceptT AppError IO) a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader Env
        , MonadError AppError
        )

instance MonadLogger AppM where
    monadLoggerLog loc source level msg = do
        Env { envLogAction, envLogLevel } <- ask
        when (level >= envLogLevel) $
            liftIO $ envLogAction loc source level (toLogStr msg)

-- | Constaint for monadic stack that has access to a database pool.
class WithDb m where
    withPool :: (Pool -> IO a) -> m a

instance WithDb AppM where
    withPool f = do
        pool <- asks envDbPool
        liftIO $ f pool

class HasSpeller m where
    runSpeller :: Text -> m SpellCheck

instance HasSpeller AppM where
    runSpeller t = do
        action <- asks envSpellerAction
        action t

runApp :: Env -> AppM a -> IO (Either AppError a)
runApp env app = runExceptT $ runReaderT (unAppM app) env

runAppAsIO :: Env -> AppM a -> IO a
runAppAsIO env app = runApp env app >>= either throwIO return

runAppAsHandler :: Env -> AppM a -> Handler a
runAppAsHandler env app = do
    res <- liftIO $ runApp env app
    liftEither $ first toHttpError res
