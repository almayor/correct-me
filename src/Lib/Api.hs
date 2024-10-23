{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DerivingVia #-}

module Lib.Api where

import Servant hiding (BasicAuth)
import Servant.Auth.Swagger 
import Data.Vector (Vector)

import Lib.Types

-- POST /users/
type RegisterAPI = "users" :>
    Summary "Register a new user" :> Description "Creates a new user and returns its URL"
        :> ReqBody '[JSON] UserReq :> PostCreated '[JSON] LocPath

type UsersAPI = "users" :> (
    -- GET /users/
    Summary "Get list of users" :> Description "Returns a list of URLs to users"
        :> Get '[JSON] (Vector LocPath) :<|>
    -- GET /users/3/
    Summary "Get a user" :> Description "Returns the details of a specific user"
        :> Capture "user_id" UserID :> Get '[JSON] User :<|>
    -- GET /users/3/phrases?open
    Summary "Get user's phrases" :> Description "Returns a list of URLs to phrases created by a specific user"
        :> Capture "user_id" UserID :> "phrases" :> QueryFlag "open" :> Get '[JSON] (Vector LocPath)
    )

type PhrasesAPI = "phrases" :> (
    -- GET /phrases?open
    Summary "Get list of phrases" :> Description "Returns a list of URLs to phrases"
        :> QueryFlag "open" :> Get '[JSON] (Vector LocPath) :<|>
    -- POST /phrases
    Summary "Create a new phrase" :> Description "Creates a new phrase and returns its URL"
        :> ReqBody '[JSON] PhraseReq :> PostCreated '[JSON] LocPath :<|>
    -- GET /phrases/24
    Summary "Get a phrase" :> Description "Returns the details of a specific phrase"
        :> Capture "phrase_id" PhraseID :> Get '[JSON] Phrase :<|>
    -- GET /phrases/24/alternatives
    Summary "Get alternatives for a phrase" :> Description "Returns a list of URLs to alternatives for a specific phrase"
        :> Capture "phrase_id" PhraseID :> "alternatives" :> Get '[JSON] (Vector LocPath) :<|>
    -- POST /phrases/24/alternatives
    Summary "Create an alternative for a phrase" :> Description "Creates a new alternative for a specific phrase and returns its URL"
        :> Capture "phrase_id" PhraseID :> "alternatives" :> ReqBody '[JSON] AlternativeReq :> PostCreated '[JSON] LocPath
    )

type AlternativesAPI = "alternatives" :> (
    -- GET /alternatives/301?author_id=3
    Summary "Get list of alternatives by author" :> Description "Returns a list of URLs to alternatives filtered by author ID"
        :> QueryParam "author_id" UserID :> Get '[JSON] (Vector LocPath) :<|>
    -- GET /alternatives/301
    Summary "Get an alternative" :> Description "Returns the details of a specific alternative"
        :> Capture "alternativeId" AlternativeID :> Get '[JSON] Alternative :<|>
    -- PUT /alternatives/301/choose
    Summary "Choose an alternative" :> Description "Marks a specific alternative as chosen and returns the URL to the phrase"
        :> Capture "alternativeId" AlternativeID :> "choose" :> Patch '[JSON] LocPath
    )

type PublicAppAPI = RegisterAPI
type ProtectedAppAPI = UsersAPI :<|> PhrasesAPI :<|> AlternativesAPI
type AppAPI = "api" :> (PublicAppAPI :<|> Auth '[BasicAuth, JWT] User :> ProtectedAppAPI) 

userId2Loc :: UserID -> LocPath
userId2Loc = LocPath . ("/api/users/" ++) . show

phraseId2Loc :: PhraseID -> LocPath
phraseId2Loc = LocPath . ("/api/phrases/" ++) . show

alternativeId2Loc :: AlternativeID -> LocPath
alternativeId2Loc = LocPath . ("/api/alternatives/" ++) . show
