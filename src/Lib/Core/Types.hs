{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DerivingVia #-}
module Lib.Core.Types where

import Data.Text (Text)
import Data.Int (Int32)
import Data.Time (UTCTime)
import Data.Aeson (Value)
import Deriving.Aeson
import Servant (FromHttpApiData)

newtype UserName = UserName { unUserName :: Text }
    deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData)

newtype PasswordHash = PasswordHash { unPasswordHash :: Text }
    deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData)

newtype PasswordPlain = PasswordPlain { unPasswordPlain :: Text }
    deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData)

newtype UserID = UserID { unUserId :: Int32 }
    deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData)

newtype PhraseID = PhraseID { unPhraseId :: Int32 }
    deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData)

newtype AlternativeID = AlternativeID { unAlternativeId :: Int32 }
    deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData)

newtype SpellCheck = SpellCheck { unSpellcheck :: Value }
    deriving newtype (Show, FromJSON, ToJSON)

-- location path, e.g. "/api/users/3"
newtype LocPath = LocPath String
    deriving newtype (Show, FromJSON, ToJSON)

data User = User
    { userId        :: UserID
    , userUserName  :: UserName
    , userCreatedAt :: UTCTime
    }
    deriving (Show, Generic)
    deriving (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "user", CamelToSnake]] User

data Alternative = Alternative
    { altId          :: AlternativeID
    , altAuthorId    :: UserID
    , altPhraseId    :: PhraseID
    , altText        :: Text
    , altCreatedAt   :: UTCTime
    -- , altSpellCheck  :: SpellCheck
    }
    deriving (Show, Generic)
    deriving (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "alt", CamelToSnake]] Alternative

data Phrase = Phrase
    { phraseId            :: PhraseID
    , phraseAuthorId      :: UserID
    , phraseText          :: Text
    , phraseCreatedAt     :: UTCTime
    , phraseIsOpen        :: Bool
    , phraseChosenAltId   :: Maybe AlternativeID
    , phraseNumAlts       :: Int32
    -- , phraseSpellCheck    :: SpellCheck
    }
    deriving (Show, Generic)
    deriving (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "phrase", CamelToSnake]] Phrase

data UserReq = UserReq
    { userReqUserName :: UserName
    , userReqPassword :: PasswordPlain
    }
    deriving (Show, Generic)
    deriving (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "userReq", CamelToSnake]] UserReq

data PhraseReq = PhraseReq
    { phraseReqText :: Text
    }
    deriving (Show, Generic)
    deriving (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "phraseReq", CamelToSnake]] PhraseReq

data AlternativeReq = AlternativeReq
    { altReqText :: Text
    }
    deriving (Show, Generic)
    deriving (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "altReq", CamelToSnake]] AlternativeReq