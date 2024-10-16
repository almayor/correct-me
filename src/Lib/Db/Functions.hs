module Lib.Db.Functions
        ( execute ) where

import Hasql.Session (statement)
import Hasql.Statement (Statement (..))
import qualified Hasql.Pool as Pool

import Lib.App.Error (CanFail, liftDbError)
import Lib.App.Monad (WithDb, withPool)
    
execute :: (WithDb m, CanFail m) => Statement ps a -> ps -> m a
execute st params = do
    let session = statement params st
    result <- withPool $ \pool -> Pool.use pool session
    liftDbError result