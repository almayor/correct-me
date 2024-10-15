module Lib where

import Control.Monad.Logger
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word16)
import Hasql.Connection (settings)
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import System.Environment (lookupEnv)
import System.IO (stdout)
import Toml (TomlCodec, (.=))
import qualified Toml

import Monad
import Config

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
 

