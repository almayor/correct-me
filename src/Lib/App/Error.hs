module Lib.App.Error (
    CanFail,
    AppError(..),
    toHttpError,
    liftDbError
) where

import Control.Monad.Except (MonadError, throwError)
import Servant (ServerError, err409, err500, err404, errBody, err403, err422, err401, err400)
import Hasql.Pool (UsageError)
import Data.String (fromString)
import Control.Monad.Logger (MonadLogger)
import Control.Exception (Exception)

type CanFail m = (MonadError AppError m, MonadLogger m)

data AppError =
      NotFoundError
    | UserAlreadyExistsError
    | InvalidContent String 
    | NotTheAuthorError
    | PhraseAlreadyClosedError
    | ExternalServiceError String
    | NotAuthenticatedError
    | DbError UsageError
    | InternalError String
    deriving (Show)
    deriving anyclass (Exception)

toHttpError :: AppError -> ServerError
toHttpError NotFoundError = err404
toHttpError UserAlreadyExistsError = err409 { errBody = "User already exists" }
toHttpError (InvalidContent e) = err400 { errBody = fromString e }
toHttpError NotTheAuthorError = err403 { errBody = "Only author can do that" }
toHttpError PhraseAlreadyClosedError = err409 { errBody = "Phrase is already closed" }
toHttpError (DbError e) = err500 { errBody = fromString $ show e }
toHttpError NotAuthenticatedError = err401 { errBody = "Not authenticated" }
toHttpError (ExternalServiceError e) = err500 { errBody = fromString e }
toHttpError (InternalError e) = err500 { errBody = fromString e }

liftDbError :: CanFail m => Either UsageError a -> m a
liftDbError = either (throwError . DbError) return
