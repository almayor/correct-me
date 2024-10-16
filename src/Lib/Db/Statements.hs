{-# LANGUAGE QuasiQuotes #-}

module Lib.Db.Statements where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Tuple.All (uncurryN)
import Data.Profunctor (rmap)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Hasql.Statement (Statement (..))
import Hasql.TH (maybeStatement, singletonStatement, vectorStatement)

import Lib.Api

import Hasql.Decoders (Row)

userInsertSt :: Statement (Text, Text) EntryID
userInsertSt = 
      [singletonStatement|
        INSERT INTO users (username, password)
        VALUES ($1 :: text, $2 :: text)
        RETURNING id :: int4|]

userExistsSt :: Statement Text Bool
userExistsSt = 
      [singletonStatement|
        SELECT EXISTS(SELECT 1 FROM users WHERE username = $1 :: text) :: bool|]

userGetByUsernameSt :: Statement Text (Maybe User)
userGetByUsernameSt = rmap (uncurryN User <$>)
      [maybeStatement|
        SELECT id :: int4, username :: text, password :: text, created_at :: timestamptz
        FROM users
        WHERE username = $1 :: text|]

userGetByIdSt :: Statement EntryID (Maybe User)
userGetByIdSt = rmap (uncurryN User <$>)
      [maybeStatement|
        SELECT id :: int4, username :: text, password :: text, created_at :: timestamptz
        FROM users
        WHERE id = $1 :: int4|]

phraseGetAllSt :: Statement (Bool, Maybe EntryID) (Vector Phrase)
phraseGetAllSt = rmap (V.map (uncurryN Phrase))
  [vectorStatement|
    SELECT id :: int4, author_id :: int4, text :: text, chosen_alt_id :: int4?, is_open :: bool, created_at :: timestamptz
    FROM phrases
    WHERE (NOT $1 :: bool OR is_open = $1 :: bool)
      AND ($2 :: int4? IS NULL OR author_id = $2 :: int4?)
    ORDER BY created_at DESC|]

phrasesInsertSt :: Statement (UserID, Text) EntryID
phrasesInsertSt = 
      [singletonStatement|
        INSERT INTO phrases (author_id, text)
        VALUES ($1 :: int4, $2 :: text)
        RETURNING id :: int4|]

    
