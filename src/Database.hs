{-# LANGUAGE QuasiQuotes #-}

module Database where

import qualified Hasql.Pool as Pool
import Hasql.Pool (Pool, acquire)
import Hasql.Connection (Connection, ConnectionError, release, settings)

import Hasql.Session (QueryError, Session, run, statement)
import Hasql.Statement (Statement (..))
import Hasql.TH (maybeStatement, resultlessStatement, rowsAffectedStatement, singletonStatement, vectorStatement)
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction as Transaction (statement)
import Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..), transaction)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime)

import Lib
import Error
import Types
import Password

import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Data.Tuple.All (uncurryN)
import Data.Profunctor (rmap)

data UserDB = UserDB
    { userId        :: EntryID
    , userUsername  :: Text
    , userPwdHash   :: Text
    , userCreatedAt :: UTCTime
    } deriving (Show, Generic)

data PhraseDB = PhraseDB
    { phraseId          :: EntryID
    , phraseUserId      :: Int32
    , phraseText        :: Text
    , phraseIsOpen      :: Bool
    , phraseCreatedAt   :: UTCTime
    } deriving (Show, Generic)

data AlternativeDB = AlternativeDB
    { altId          :: EntryID
    , altUserId      :: Int32
    , altPhraseId    :: Int32
    , altText        :: Text
    , altCreatedAt   :: UTCTime
    } deriving (Show)

insertUserSt :: Statement (Text, Text) EntryID
insertUserSt = 
      [singletonStatement|
        INSERT INTO users (username, password)
        VALUES ($1 :: text, $2 :: text)
        RETURNING id :: int4|]

userExistsSt :: Statement Text Bool
userExistsSt = 
      [singletonStatement|
        SELECT EXISTS(SELECT 1 FROM users WHERE username = $1 :: text) :: bool|]

getUserByUsernameSt :: Statement Text (Maybe UserDB)
getUserByUsernameSt = rmap (uncurryN UserDB <$>)
      [maybeStatement|
        SELECT id :: int4, username :: text, password :: text, created_at :: timestamptz
        FROM users
        WHERE username = $1 :: text|]

getUserSt :: Statement Int32 (Maybe UserDB)
getUserSt = rmap (uncurryN UserDB <$>)
      [maybeStatement|
        SELECT id :: int4, username :: text, password :: text, created_at :: timestamptz
        FROM users
        WHERE id = $1 :: int4|]


-- execute :: (WithDb m) => Session a -> m (Either Pool.UsageError a)
-- execute sess = withPool $ \pool -> Pool.use pool sess

execute :: (WithDb m, CanFail m) => Statement ps a -> ps -> m a
execute st params = do
    let session = statement params st
    result <- withPool $ \pool -> Pool.use pool session
    liftDbError result


-- testSess :: App ()
-- testSess = withPool $ \pool -> do
--     let sess = insertUser "test" "test" 
--     result <- Pool.use pool sess 
--     case result of
--         Left err -> liftIO $ print err
--         Right _  -> return ()

-- liftDbError :: Either UsageError a -> App a
-- liftDbError = either (throwError . DbError) return