module Lib.Api where

import Servant
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.TH (deriveJSON, Options (fieldLabelModifier), defaultOptions)
import Text.Casing (quietSnake)
import Data.Int (Int32)
import Data.Time (UTCTime)
import Lib.Core.Password (PasswordHash)
import Data.Vector (Vector)

type EntryID = Int32
type UserID = EntryID

data UserReq = UserReq
    { userReqUsername :: Text
    , userReqPassword :: Text
    } deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop 7} ''UserReq)

data PhraseReq = PhraseReq
    { phraseReqText :: Text
    } deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop 9} ''PhraseReq)

data User = User
    { userId        :: EntryID
    , userUsername  :: Text
    , userPwdHash   :: PasswordHash
    , userCreatedAt :: UTCTime
    } deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop 4} ''User)

data Phrase = Phrase
    { phraseId          :: EntryID
    , phraseUserId      :: EntryID
    , phraseText        :: Text
    , phraseChosenAltId :: Maybe EntryID
    , phraseIsOpen      :: Bool
    , phraseCreatedAt   :: UTCTime
    } deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop 6} ''Phrase)

data Alternative = Alternative
    { altId          :: EntryID
    , altUserId      :: EntryID
    , altPhraseId    :: EntryID
    , altText        :: Text
    , altCreatedAt   :: UTCTime
    } deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop 4} ''Alternative)

type RegisterAPI = "register" :> ReqBody '[JSON] UserReq :> Post '[JSON] UserID

-- type PhraseAPI = "phrases" :> (
--          QueryParam "author_id" Int :> QueryFlag "open" :> Get '[JSON] [Phrase]
--     :<|> ReqBody '[JSON] PhraseReq :> Post '[JSON] NoContent
--     :<|> Capture "id" Int :> "alternatives" :> (
--              Get '[JSON] [Alternative]
--         :<|> ReqBody '[JSON] AlternativeReq :> Post '[JSON] NoContent
--         :<|> Capture "id" Int :> "approve" :> Put '[JSON] NoContent
--         )
--     )

-- type PhraseAPI = "phrases" :>
--     (    QueryParam "author_id" Int :> QueryFlag "open" :> Get '[JSON] [PhraseResp]
--     :<|> ReqBody '[JSON] PhraseReq :> Post '[JSON] NoContent
--     )

type PhrasesAPI = "phrases" :> (
         QueryParam "author_id" UserID :> QueryFlag "open" :> Get '[JSON] (Vector Phrase)
    :<|> ReqBody '[JSON] PhraseReq :> Post '[JSON] EntryID
    )

type API = RegisterAPI :<|> BasicAuth "basic-auth" User :> PhrasesAPI 

