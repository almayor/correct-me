{-# LANGUAGE QuasiQuotes #-}

module Lib.Db.Statements where

import Data.Text (Text)
import Data.Tuple.All (uncurryN)
import Data.Profunctor (rmap)
import Data.Vector (Vector)

import Hasql.Statement (Statement (..))
import Hasql.TH (maybeStatement, singletonStatement, vectorStatement)

import Lib.Api
import Lib.Core.Password (PasswordHash)

userExistsSt :: Statement Text Bool
userExistsSt = 
      [singletonStatement|
        SELECT EXISTS(SELECT 1 FROM users WHERE username = $1 :: text) :: bool|]

userGetByUsernameSt :: Statement Text (Maybe User)
userGetByUsernameSt = rmap (uncurryN User <$>)
      [maybeStatement|
        SELECT id :: int4, username :: text, created_at :: timestamptz
        FROM users
        WHERE username = $1 :: text|]

userGetPasswordSt :: Statement EntryID PasswordHash
userGetPasswordSt = 
      [singletonStatement|
        SELECT password :: text
        FROM users
        WHERE id = $1 :: int4|]

userGetSt :: Statement UserID (Maybe User)
userGetSt = rmap (uncurryN User <$>)
      [maybeStatement|
        SELECT id :: int4, username :: text, created_at :: timestamptz
        FROM users
        WHERE id = $1 :: int4|]

usersListSt :: Statement () (Vector UserID)
usersListSt =
  [vectorStatement|
    SELECT
      id :: int4
    FROM users|]

userInsertSt :: Statement (Text, Text) EntryID
userInsertSt = 
      [singletonStatement|
        INSERT INTO users (username, password)
        VALUES ($1 :: text, $2 :: text)
        RETURNING id :: int4|]

phraseGetSt :: Statement EntryID (Maybe Phrase)
phraseGetSt = rmap (uncurryN Phrase <$>)
      [maybeStatement|
        SELECT
          id :: int4,
          author_id :: int4,
          text :: text,
          created_at :: timestamptz,
          is_open :: bool,
          chosen_alt_id :: int4?,
          (SELECT COUNT(*) FROM alternatives WHERE phrase_id = $1 :: int4) :: int4
        FROM phrases
        WHERE id = $1 :: int4|]

phraseListSt :: Statement (Bool, Maybe UserID) (Vector EntryID)
phraseListSt =
  [vectorStatement|
    SELECT
      id :: int4
    FROM phrases
    WHERE (NOT $1 :: bool OR is_open = $1 :: bool)
      AND ($2 :: int4? IS NULL OR author_id = $2 :: int4?)
    ORDER BY created_at DESC|]

phraseInsertSt :: Statement (UserID, Text) EntryID
phraseInsertSt = 
      [singletonStatement|
        INSERT INTO phrases (author_id, text)
        VALUES ($1 :: int4, $2 :: text)
        RETURNING id :: int4|]

phraseSetChosenAltSt :: Statement (EntryID, EntryID) ()
phraseSetChosenAltSt = 
      [singletonStatement|
        UPDATE phrases
        SET chosen_alt_id = $2 :: int4, is_open = FALSE
        WHERE id = $1 :: int4|]

alternativeGetSt :: Statement EntryID (Maybe Alternative)
alternativeGetSt = rmap (uncurryN Alternative <$>)
      [maybeStatement|
        SELECT
          id :: int4,
          author_id :: int4,
          phrase_id :: int4,
          text :: text,
          created_at :: timestamptz
        FROM alternatives
        WHERE id = $1 :: int4|]
    
alternativeListSt :: Statement EntryID (Vector EntryID)
alternativeListSt =
  [vectorStatement|
    SELECT
      id :: int4
    FROM alternatives
    WHERE phrase_id = $1 :: int4
    ORDER BY created_at DESC|]

alternativeInsertSt :: Statement (UserID, EntryID, Text) EntryID
alternativeInsertSt = 
      [singletonStatement|
        INSERT INTO alternatives (author_id, phrase_id, text)
        VALUES ($1 :: int4, $2 :: int4, $3 :: text)
        RETURNING id :: int4|]



    
