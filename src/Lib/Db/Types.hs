module Lib.Db.Types
        ( UserDB(..)
        , PhraseDB(..)
        , AlternativeDB(..)
        ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Lib.Core.Password
import Lib.Api

data UserDB = UserDB
    { userId        :: EntryID
    , userUsername  :: Text
    , userPwdHash   :: PasswordHash
    , userCreatedAt :: UTCTime
    } deriving (Show, Generic)

data PhraseDB = PhraseDB
    { phraseId          :: EntryID
    , phraseUserId      :: EntryID
    , phraseText        :: Text
    , phraseIsOpen      :: Bool
    , phraseCreatedAt   :: UTCTime
    } deriving (Show, Generic)

data AlternativeDB = AlternativeDB
    { altId          :: EntryID
    , altUserId      :: EntryID
    , altPhraseId    :: EntryID
    , altText        :: Text
    , altCreatedAt   :: UTCTime
    } deriving (Show)
