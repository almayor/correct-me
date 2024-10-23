{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Lib.Types where

import Data.Text (Text)
import Data.Int (Int32)
import Data.Time (UTCTime)
import Data.Aeson (Value, FromJSON, ToJSON)
import qualified Data.Aeson.TH as TH (defaultOptions, deriveJSON, omitNothingFields, fieldLabelModifier)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Data.Swagger (ToParamSchema, NamedSchema (..), SwaggerType (..), HasType (type_), AdditionalProperties (..))
import Data.Swagger.Schema
import Data.Typeable (Typeable)
import Data.Swagger.Lens (HasAdditionalProperties(..), HasExample (example))
import Data.String (IsString)
import Control.Lens ((?~), (&))

import Servant (FromHttpApiData, ToHttpApiData)
import Servant.Auth.Server (ToJWT, FromJWT)
import GHC.Generics (Generic)

import Lib.Core.Utils (modifyLabel)

newtype UserName = UserName { unUserName :: Text }
    deriving (Generic)
    deriving newtype (Eq, Show, IsString, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, ToSchema, ToParamSchema)

newtype PasswordHash = PasswordHash { unPasswordHash :: Text }
    deriving (Generic)
    deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, ToSchema)

newtype PasswordPlain = PasswordPlain { unPasswordPlain :: Text }
    deriving (Generic)
    deriving newtype (Eq, Show, IsString, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, ToSchema)

newtype UserID = UserID { unUserId :: Int32 }
    deriving (Generic)
    deriving newtype (Eq, Show, Ord, Bounded, Enum, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, ToSchema, ToParamSchema)

newtype PhraseID = PhraseID { unPhraseId :: Int32 }
    deriving (Generic)
    deriving newtype (Eq, Show, Ord, Bounded, Enum, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, ToSchema, ToParamSchema)

newtype AlternativeID = AlternativeID { unAlternativeId :: Int32 }
    deriving (Generic)
    deriving newtype (Eq, Show, Ord, Bounded, Enum, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData, ToSchema, ToParamSchema)

newtype SpellCheckID = SpellCheckID { unSpellCheckId :: Int32 }
    deriving (Generic)
    deriving newtype (Eq, Show, Ord, Bounded, Enum, FromJSON, ToJSON)

newtype SpellCheck = SpellCheck { unSpellCheck :: Value }
    deriving (Generic)
    deriving newtype (Show, FromJSON, ToJSON)

instance ToSchema SpellCheck where
    declareNamedSchema _ = return $ NamedSchema (Just "SpellCheck") $ mempty
      & type_ ?~ SwaggerObject
      & additionalProperties ?~ AdditionalPropertiesAllowed True
      & example ?~ "[{json-response-from-speller}]"

-- location path, e.g. "/api/users/3"
newtype LocPath = LocPath String
    deriving (Generic)
    deriving newtype (Eq, Show, IsString, FromJSON, ToJSON)

instance ToSchema LocPath where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "LocPath") $ mempty
      & type_ ?~ SwaggerString
      & example ?~ "/api/path-to-resource"

data User = User
    { userId        :: UserID
    , userUserName  :: UserName
    , userCreatedAt :: UTCTime
    }
    deriving (Show, Generic, Typeable)

$(TH.deriveJSON TH.defaultOptions { TH.fieldLabelModifier = modifyLabel "user", TH.omitNothingFields = True } ''User)

instance ToSchema User where
    declareNamedSchema = genericDeclareNamedSchemaUnrestricted
        defaultSchemaOptions { fieldLabelModifier = modifyLabel "user" }

instance FromJWT User where
instance ToJWT User where

data Alternative = Alternative
    { altId          :: AlternativeID
    , altAuthorId    :: UserID
    , altPhraseId    :: PhraseID
    , altText        :: Text
    , altCreatedAt   :: UTCTime
    , altSpellCheck  :: SpellCheck
    }
    deriving (Show, Generic)

$(TH.deriveJSON TH.defaultOptions { TH.fieldLabelModifier = modifyLabel "alt" } ''Alternative)

instance ToSchema Alternative where
    declareNamedSchema = genericDeclareNamedSchemaUnrestricted
        defaultSchemaOptions { fieldLabelModifier = modifyLabel "alt" }

data Phrase = Phrase
    { phraseId            :: PhraseID
    , phraseAuthorId      :: UserID
    , phraseText          :: Text
    , phraseCreatedAt     :: UTCTime
    , phraseIsOpen        :: Bool
    , phraseChosenAltId   :: Maybe AlternativeID
    , phraseNumAlts       :: Int32
    , phraseSpellCheck    :: SpellCheck
    }
    deriving (Show, Generic)

$(TH.deriveJSON TH.defaultOptions { TH.fieldLabelModifier = modifyLabel "phrase" } ''Phrase)

instance ToSchema Phrase where
    declareNamedSchema = genericDeclareNamedSchemaUnrestricted
        defaultSchemaOptions { fieldLabelModifier = modifyLabel "phrase" }

data UserReq = UserReq
    { userReqUserName :: UserName
    , userReqPassword :: PasswordPlain
    }
    deriving (Show, Generic)

$(TH.deriveJSON TH.defaultOptions { TH.fieldLabelModifier = modifyLabel "userReq" } ''UserReq)

instance ToSchema UserReq where
    declareNamedSchema = genericDeclareNamedSchemaUnrestricted
        defaultSchemaOptions { fieldLabelModifier = modifyLabel "userReq" }

data PhraseReq = PhraseReq
    { phraseReqText :: Text
    }
    deriving (Show, Generic)

$(TH.deriveJSON TH.defaultOptions { TH.fieldLabelModifier = modifyLabel "phraseReq" } ''PhraseReq)

instance ToSchema PhraseReq where
    declareNamedSchema = genericDeclareNamedSchemaUnrestricted
        defaultSchemaOptions { fieldLabelModifier = modifyLabel "phraseReq" }

data AlternativeReq = AlternativeReq
    { altReqText :: Text
    }
    deriving (Show, Generic)

$(TH.deriveJSON TH.defaultOptions { TH.fieldLabelModifier = modifyLabel "altReq" } ''AlternativeReq)

instance ToSchema AlternativeReq where
    declareNamedSchema = genericDeclareNamedSchemaUnrestricted
        defaultSchemaOptions { fieldLabelModifier = modifyLabel "altReq" }

bs2UserName :: ByteString -> UserName
bs2UserName = UserName . decodeUtf8

bs2Password :: ByteString -> PasswordPlain
bs2Password = PasswordPlain . decodeUtf8

