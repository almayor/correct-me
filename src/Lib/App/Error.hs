module Lib.App.Error where

import Control.Monad.Except (MonadError, throwError)
import Servant (ServerError, err409, err500, err404, errBody, err403)
import Hasql.Pool (UsageError)
import Data.String (fromString)
import Control.Monad.Logger (MonadLogger, logError)

type CanFail m = (MonadError ServerError m, MonadLogger m)

notFoundError :: CanFail m => m a
notFoundError = throwError err404

userAlreadyExistsError :: CanFail m => m a
userAlreadyExistsError = throwError $ err409 { errBody = "User already exists" }

notTheAuthorError :: CanFail m => m a
notTheAuthorError = throwError $ err403 { errBody = "Only author can choose alternative" }

alreadyClosedError :: CanFail m => m a
alreadyClosedError = throwError $ err409 { errBody = "Phrase is already closed" }

inconsistentDataError :: CanFail m => m a
inconsistentDataError = throwError $ err500 { errBody = "Inconsistent data" }

dbError :: CanFail m => UsageError -> m a
dbError e = do
    $(logError) $ "Database error: " <> fromString (show e)
    throwError $ err500 { errBody = "Database error" }

internalError :: CanFail m => String -> m a
internalError e = do
    $(logError) $ "Server error: " <> fromString e
    throwError $ err500 { errBody = fromString e }

liftDbError :: CanFail m => Either UsageError a -> m a
liftDbError = either dbError return

liftServerError :: CanFail m => Either ServerError a -> m a
liftServerError = either throwError return
