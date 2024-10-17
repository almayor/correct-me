{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DerivingVia #-}

module Lib.Api where

import Servant
import Data.Vector (Vector)

import Lib.Core.Types

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
    Capture "phrase_id" PhraseID :> Get '[JSON] Phrase :<|>
    -- GET /phrases/24/alternatives
    Capture "phrase_id" PhraseID :> "alternatives" :> Get '[JSON] (Vector LocPath) :<|>
    -- POST /phrases/24/alternatives
    Capture "phrase_id" PhraseID :> "alternatives" :> ReqBody '[JSON] AlternativeReq :> Post '[JSON] LocPath
    )

type AlternativeAPI = "alternatives" :> (
    -- GET /alternatives/301?author_id=3
    QueryParam "author_id" UserID :> Get '[JSON] (Vector LocPath) :<|>
    -- GET /alternatives/301
    Capture "alternativeId" AlternativeID :> Get '[JSON] Alternative :<|>
    -- PUT /alternatives/301/choose
    Capture "alternativeId" AlternativeID :> "choose" :> Put '[JSON] LocPath
    )

type PublicAPI = RegisterAPI
type ProtectedAPI = UsersAPI :<|> PhraseAPI :<|> AlternativeAPI
type API = "api" :> (PublicAPI :<|> BasicAuth "basic-auth" User :> ProtectedAPI) 

userId2Loc :: UserID -> LocPath
userId2Loc = LocPath . ("/api/users/" ++) . show

phraseId2Loc :: PhraseID -> LocPath
phraseId2Loc = LocPath . ("/api/phrases/" ++) . show

alternativeId2Loc :: AlternativeID -> LocPath
alternativeId2Loc = LocPath . ("/api/alternatives/" ++) . show
