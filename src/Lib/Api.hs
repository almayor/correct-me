{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Lib.Api where

import Servant
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.TH (deriveJSON, Options (fieldLabelModifier), defaultOptions)
import Text.Casing (quietSnake)
import Data.Int (Int32)
import Data.Time (UTCTime)
import Data.Vector (Vector)

type EntryID = Int32
type UserID = EntryID
type LocPath = String

data User = User
    { userId        :: EntryID
    , userUsername  :: Text
    , userCreatedAt :: UTCTime
    } deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop 4} ''User)

data Alternative = Alternative
    { altId          :: EntryID
    , altAuthorId    :: UserID
    , altPhraseId    :: EntryID
    , altText        :: Text
    , altCreatedAt   :: UTCTime
    } deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop 3} ''Alternative)

data Phrase = Phrase
    { phraseId            :: EntryID
    , phraseAuthorId      :: UserID
    , phraseText          :: Text
    , phraseCreatedAt     :: UTCTime
    , phraseIsOpen        :: Bool
    , phraseChosenAltId   :: Maybe EntryID
    , phraseNumAlts       :: EntryID
    } deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop 6} ''Phrase)

data UserReq = UserReq
    { userReqUsername :: Text
    , userReqPassword :: Text
    } deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop 7} ''UserReq)

data PhraseReq = PhraseReq
    { phraseReqText :: Text
    } deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop 9} ''PhraseReq)

data AlternativeReq = AlternativeReq
    { altReqText :: Text
    } deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop 6} ''AlternativeReq)

-- POST /users/
type RegisterAPI = "users" :> ReqBody '[JSON] UserReq :> Post '[JSON] LocPath

type UsersAPI = "users" :> (
    -- GET /users/
    Get '[JSON] (Vector LocPath) :<|>
    -- GET /users/3/
    Capture "user_id" UserID :> Get '[JSON] User :<|>
    -- GET /users/3/phrases?open
    Capture "user_id" UserID :> "phrases" :> QueryFlag "open" :> Get '[JSON] (Vector LocPath)
    )

type PhraseAPI = "phrases" :> (
    -- GET /phrases?open
    QueryFlag "open" :> Get '[JSON] (Vector LocPath) :<|>
    -- POST /phrases
    ReqBody '[JSON] PhraseReq :> Post '[JSON] LocPath :<|>
    -- GET /phrases/24
    Capture "phrase_id" EntryID :> Get '[JSON] Phrase :<|>
    -- GET /phrases/24/alternatives
    Capture "phrase_id" EntryID :> "alternatives" :> Get '[JSON] (Vector LocPath) :<|>
    -- POST /phrases/24/alternatives
    Capture "phrase_id" EntryID :> "alternatives" :> ReqBody '[JSON] AlternativeReq :> Post '[JSON] LocPath
    )

type AlternativeAPI = "alternatives" :> (
    -- GET /alternatives/301?author_id=3
    QueryParam "author_id" UserID :> Get '[JSON] (Vector LocPath) :<|>
    -- GET /alternatives/301
    Capture "alternativeId" EntryID :> Get '[JSON] Alternative :<|>
    -- PUT /alternatives/301/choose
    Capture "alternativeId" EntryID :> "choose" :> Put '[JSON] LocPath
    )

type PublicAPI = RegisterAPI
type ProtectedAPI = UsersAPI :<|> PhraseAPI :<|> AlternativeAPI
type API = "api" :> (PublicAPI :<|> BasicAuth "basic-auth" User :> ProtectedAPI) 

userId2Loc :: UserID -> LocPath
userId2Loc = ("/api/users/" ++) . show

phraseId2Loc :: EntryID -> LocPath
phraseId2Loc = ("/api/phrases/" ++) . show

alternativeId2Loc :: EntryID -> LocPath
alternativeId2Loc = ("/api/alternatives/" ++) . show
