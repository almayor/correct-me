{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DerivingVia #-}
module Lib.Core.Types where

import Data.Text (Text)
import Data.Int (Int32)
import Data.Time (UTCTime)
import Data.Aeson (Value, Object)
import Deriving.Aeson
import Servant (FromHttpApiData)
import Data.Swagger (ToSchema, ToParamSchema)
import Data.Typeable (Typeable)
import Servant.Auth.Server (ToJWT)
import Servant.Auth.JWT (FromJWT)

newtype UserName = UserName { unUserName :: Text }
    deriving (Generic)
    deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData)
    deriving anyclass (ToSchema, ToParamSchema)

newtype PasswordHash = PasswordHash { unPasswordHash :: Text }
    deriving (Generic)
    deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData)
    deriving anyclass (ToSchema)

newtype PasswordPlain = PasswordPlain { unPasswordPlain :: Text }
    deriving (Generic)
    deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData)
    deriving anyclass (ToSchema)

newtype UserID = UserID { unUserId :: Int32 }
    deriving (Generic)
    deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData)
    deriving anyclass (ToSchema, ToParamSchema)

newtype PhraseID = PhraseID { unPhraseId :: Int32 }
    deriving (Generic)
    deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData)
    deriving anyclass (ToSchema, ToParamSchema)

newtype AlternativeID = AlternativeID { unAlternativeId :: Int32 }
    deriving (Generic)
    deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData)
    deriving anyclass (ToSchema, ToParamSchema)

newtype SpellCheckID = SpellCheckID { unSpellCheckId :: Int32 }
    deriving (Generic)
    deriving newtype (Eq, Show, FromJSON, ToJSON)

newtype SpellCheck = SpellCheck { unSpellCheck :: Value }
    deriving (Generic)
    deriving newtype (Show, FromJSON, ToJSON)
    -- deriving anyclass (ToSchema)

-- location path, e.g. "/api/users/3"
newtype LocPath = LocPath String
    deriving (Generic)
    deriving newtype (Show, FromJSON, ToJSON)
    deriving anyclass (ToSchema, ToParamSchema)

data User = User
    { userId        :: UserID
    , userUserName  :: UserName
    , userCreatedAt :: UTCTime
    }
    deriving (Show, Generic, Typeable)
    deriving anyclass (ToSchema)
    deriving (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "user", CamelToSnake]] User

instance FromJWT User where
instance ToJWT User where

data Alternative = Alternative
    { altId          :: AlternativeID
    , altAuthorId    :: UserID
    , altPhraseId    :: PhraseID
    , altText        :: Text
    , altCreatedAt   :: UTCTime
    -- , altSpellCheck  :: SpellCheck
    }
    deriving (Show, Generic)
    deriving anyclass (ToSchema)
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
    deriving anyclass (ToSchema)
    deriving (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "phrase", CamelToSnake]] Phrase

data UserReq = UserReq
    { userReqUserName :: UserName
    , userReqPassword :: PasswordPlain
    }
    deriving (Show, Generic)
    deriving anyclass (ToSchema)
    deriving (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "userReq", CamelToSnake]] UserReq
  
data PhraseReq = PhraseReq
    { phraseReqText :: Text
    }
    deriving (Show, Generic)
    deriving anyclass (ToSchema)
    deriving (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "phraseReq", CamelToSnake]] PhraseReq

data AlternativeReq = AlternativeReq
    { altReqText :: Text
    }
    deriving (Show, Generic)
    deriving anyclass (ToSchema)
    deriving (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "altReq", CamelToSnake]] AlternativeReq