-- | Helper functions to create and drop database from @.sql@ files.
module Lib.Db.Schema
    ( prepareDb
    , prepareSeededDb
    ) where

import Lib.Db.Functions
import Lib.App (WithDb)
import Lib.App.Error (CanFail)
import Control.Monad.IO.Class (MonadIO (liftIO))

prepareDb :: (WithDb m, CanFail m, MonadIO m) => m ()
prepareDb = teardownDb >> setupDb

prepareSeededDb :: (WithDb m, CanFail m, MonadIO m) => m ()
prepareSeededDb = teardownDb >> setupDb >> seedDB

-- | Create tables from the @sql/schema.sql@ file.
setupDb :: (WithDb m, CanFail m, MonadIO m) => m ()
setupDb = executeFile "sql/schema.sql"

-- | Seed tables from the @sql/seed.sql@ file.
seedDB :: (WithDb m, CanFail m, MonadIO m) => m ()
seedDB = executeFile "sql/seed.sql"

-- | Create tables from the @sql/schema.sql@ file.
teardownDb :: (WithDb m, CanFail m, MonadIO m) => m ()
teardownDb = executeFile "sql/drop.sql"

executeFile :: (WithDb m, CanFail m, MonadIO m) => FilePath -> m ()
executeFile path = do
    content <- liftIO $ readFile path
    executeRaw content
