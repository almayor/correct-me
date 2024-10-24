{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib.Docs (DocsAPI, docsServer, docsMarkdown) where

import Data.Aeson (Value, decode)
import Data.Time (UTCTime(..), parseTimeOrError, defaultTimeLocale)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (pack)
import Network.Wai (responseLBS)
import Network.HTTP.Types (status200)
import Servant
import Servant.Docs
import Servant.Auth.Docs ()

import Lib.Api (AppAPI)
import Lib.Types

instance ToCapture (Capture "user_id" UserID) where
  toCapture _ = DocCapture "user_id" "The ID of the user"

instance ToCapture (Capture "phrase_id" PhraseID) where
    toCapture _ = DocCapture "phrase_id" "The ID of the phrase"

instance ToCapture (Capture "alternativeId" AlternativeID) where
    toCapture _ = DocCapture "alternativeId" "The ID of the alternative"

instance ToParam (QueryParam "author_id" UserID) where
    toParam _ = DocQueryParam "author_id" ["1", "2", "3", "..."] "Filter alternatives by ID of their author" Normal

instance ToParam (QueryFlag "open") where
    toParam _ = DocQueryParam "open" [""] "Filter by open phrases (for which no alternative has been chosen)" Flag

instance ToSample LocPath where
    toSamples _ = [("A uri leading to a user with ID equal to 3", LocPath "/api/users/3"),
                   ("A uri leading to a phrase with ID equal to 24", LocPath "/api/phrases/24"),
                   ("A uri leading to an alternative with ID equal to 301", LocPath "/api/alternatives/301")]

instance ToSample (V.Vector LocPath) where
    toSamples _ = singleSample $ V.fromList
        [ "/api/alternatives/301"
        , "/api/alternatives/305"
        , "/api/alternatives/323"
        ]

instance ToSample User where
    toSamples _ = singleSample
        User {
          userId = UserID 3
        , userUserName = "John Doe"
        , userCreatedAt = parseTime "2021-01-01 14:30:00+0000"
        }

instance ToSample Phrase where
    toSamples _ = [
          ("An open phrase ", sampleOpenPhrase)
        , ("A closed phrase", sampleClosedPhrase)
        ]

sampleOpenPhrase :: Phrase
sampleOpenPhrase = Phrase
    { phraseId = PhraseID 1
    , phraseAuthorId = UserID 123
    , phraseText = "This is a sample phrase."
    , phraseCreatedAt = parseTime "2022-08-10 18:00:00+0000"
    , phraseIsOpen = True
    , phraseChosenAltId = Nothing
    , phraseNumAlts = 1
    , phraseSpellCheck = SpellCheck $ parseJSON "[]" 
    }

sampleClosedPhrase :: Phrase
sampleClosedPhrase = Phrase
    { phraseId = PhraseID 2
    , phraseAuthorId = UserID 123
    , phraseText = "Phrase with a speling error"
    , phraseCreatedAt = parseTime "2023-10-01 18:00:10+0000"
    , phraseIsOpen = False
    , phraseChosenAltId = Just $ AlternativeID 1012
    , phraseNumAlts = 3
    , phraseSpellCheck = SpellCheck $ parseJSON "[{\"code\":1,\"pos\":14,\"row\":0,\"col\":14,\"len\":7,\"word\":\"speling\",\"s\":[\"spelling\"]}]"
    }

instance ToSample Alternative where
    toSamples _ = singleSample
        Alternative {
          altId = AlternativeID 301
        , altAuthorId = UserID 3
        , altPhraseId = PhraseID 24
        , altText = "This is an altrnative to a frase."
        , altCreatedAt = parseTime "2023-05-15 12:45:00+0000"
        , altSpellCheck = SpellCheck $ parseJSON "[{\"code\":1,\"pos\":11,\"row\":0,\"col\":11,\"len\":10,\"word\":\"altrnative\",\"s\":[\"alternative\"]},{\"code\":1,\"pos\":27,\"row\":0,\"col\":27,\"len\":5,\"word\":\"prase\",\"s\":[\"phrase\",\"place\",\"praise\",\"price\"]}]"
        }

instance ToSample UserReq where
    toSamples _ = singleSample $
        UserReq "New User" "s3cur3P@ssw0rd!"

instance ToSample PhraseReq where
    toSamples _ = singleSample $
        PhraseReq "This is a new phrase."

instance ToSample AlternativeReq where
    toSamples _ = singleSample $
        AlternativeReq "This is a new alternative."

parseJSON :: String -> Value 
parseJSON = fromMaybe undefined . decode . fromString

parseTime :: String -> UTCTime
parseTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S%z"

docsMarkdown :: String
docsMarkdown = markdown $ docsWithIntros [intro] (Proxy @AppAPI)
    where intro = DocIntro "correct-me" ["Submitting, reviewing, and improving message phrasing with built-in spellcheck."]

type DocsAPI = "docs" :> Raw

docsServer :: Server DocsAPI
docsServer = Tagged serveDocs
    where
    docsBS = encodeUtf8 . pack $ docsMarkdown
    serveDocs _ respond' = respond' $ responseLBS status200 [plain] docsBS
    plain = ("Content-Type", "text/plain")
