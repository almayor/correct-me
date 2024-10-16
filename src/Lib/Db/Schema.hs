-- | Helper functions to create and drop database from @.sql@ files.
module Lib.Db.Schema
    ( prepareDb
    ) where

import Lib.Db.Functions
import Lib.App (WithDb)
import Lib.App.Error (CanFail)

prepareDb :: (WithDb m, CanFail m) => m ()
prepareDb = undefined

-- | Create tables from the @sql/schema.sql@ file and seed data.
setupDb :: (WithDb m, CanFail m) => m ()
setupDb = undefined

-- | Create tables from the @sql/schema.sql@ file.
teardownDb :: (WithDb m, CanFail m) => m ()
teardownDb = undefined
