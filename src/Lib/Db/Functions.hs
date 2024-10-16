module Lib.Db.Functions
        ( execute
        , executeRaw 
        ) where

import Hasql.Session (statement, sql)
import Hasql.Statement (Statement (..))
import qualified Hasql.Pool as Pool
import Data.ByteString.UTF8 as BS

import Lib.App.Error (CanFail, liftDbError)
import Lib.App.Monad (WithDb, withPool)
    
execute :: (WithDb m, CanFail m) => Statement ps a -> ps -> m a
execute st params = do
    let session = statement params st
    result <- withPool $ \pool -> Pool.use pool session
    liftDbError result

executeRaw :: (WithDb m, CanFail m) => String -> m ()
executeRaw s = do
    let session = sql $ BS.fromString s
    result <- withPool $ \pool -> Pool.use pool session
    liftDbError result
