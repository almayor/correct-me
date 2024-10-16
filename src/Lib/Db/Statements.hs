{-# LANGUAGE QuasiQuotes #-}

module Lib.Db.Statements where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Tuple.All (uncurryN)
import Data.Profunctor (rmap)

import Hasql.Statement (Statement (..))
import Hasql.TH (maybeStatement, singletonStatement)

import Lib.Db.Types
import Lib.Api

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

userGetByUsernameSt :: Statement Text (Maybe UserDB)
userGetByUsernameSt = rmap (uncurryN UserDB <$>)
      [maybeStatement|
        SELECT id :: int4, username :: text, password :: text, created_at :: timestamptz
        FROM users
        WHERE username = $1 :: text|]

userGetByIdSt :: Statement EntryID (Maybe UserDB)
userGetByIdSt = rmap (uncurryN UserDB <$>)
      [maybeStatement|
        SELECT id :: int4, username :: text, password :: text, created_at :: timestamptz
        FROM users
        WHERE id = $1 :: int4|]

    
