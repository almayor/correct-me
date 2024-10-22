{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}

module Lib.Db.Statements (
    userExistsByNameSt,
    userGetByUsernameSt,
    userGetPasswordSt,
    userGetSt,
    usersListSt,
    userInsertSt,
    phraseGetSt,
    phraseListSt,
    phraseInsertSt,
    phraseSetChosenAltSt,
    alternativeGetSt,
    alternativeListByPhraseSt,
    alternativeListSt,
    alternativeInsertSt,
    spellCheckInsertSt
) where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Functor.Contravariant (contramap)
import Data.Tuple.Select (sel1, sel2, sel3, sel4)

import Hasql.Statement (Statement (..))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import Lib.Core.Types

userExistsByNameSt :: Statement UserName Bool
userExistsByNameSt = Statement sql encoder decoder True
  where
    sql = 
      "SELECT EXISTS(SELECT 1 FROM users WHERE username = $1 :: text) :: bool"
    encoder = contramap unUserName (E.param (E.nonNullable E.text))
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

userGetByUsernameSt :: Statement UserName (Maybe User)
userGetByUsernameSt = Statement sql encoder decoder True
  where
    sql = 
      "SELECT id :: int4, username :: text, created_at :: timestamptz \
      \FROM users \
      \WHERE username = $1 :: text"
    encoder = contramap unUserName (E.param (E.nonNullable E.text))
    decoder = D.rowMaybe $ User
      <$> (UserID <$> D.column (D.nonNullable D.int4))
      <*> (UserName <$> D.column (D.nonNullable D.text))
      <*> D.column (D.nonNullable D.timestamptz)

userGetPasswordSt :: Statement UserID PasswordHash
userGetPasswordSt = Statement sql encoder decoder True
  where
    sql = 
      "SELECT password :: text \
      \FROM users \
      \WHERE id = $1 :: int4"
    encoder = contramap unUserId (E.param (E.nonNullable E.int4))
    decoder = D.singleRow (PasswordHash <$> D.column (D.nonNullable D.text))

userGetSt :: Statement UserID (Maybe User)
userGetSt = Statement sql encoder decoder True
  where
    sql = 
      "SELECT id :: int4, username :: text, created_at :: timestamptz \
      \FROM users \
      \WHERE id = $1 :: int4"
    encoder = contramap unUserId (E.param (E.nonNullable E.int4))
    decoder = D.rowMaybe $ User
      <$> (UserID <$> D.column (D.nonNullable D.int4))
      <*> (UserName <$> D.column (D.nonNullable D.text))
      <*> D.column (D.nonNullable D.timestamptz)

usersListSt :: Statement () (Vector UserID)
usersListSt = Statement sql encoder decoder True
  where
    sql = 
      "SELECT id :: int4 \
      \FROM users"
    encoder = E.noParams
    decoder = D.rowVector (UserID <$> D.column (D.nonNullable D.int4))

userInsertSt :: Statement (UserName, PasswordHash) UserID
userInsertSt = Statement sql encoder decoder True
  where
    sql = 
      "INSERT INTO users (username, password) \
      \VALUES ($1 :: text, $2 :: text) \
      \RETURNING id :: int4"
    encoder = contramap (unUserName . sel1) (E.param (E.nonNullable E.text))
           <> contramap (unPasswordHash . sel2) (E.param (E.nonNullable E.text))
    decoder = D.singleRow (UserID <$> D.column (D.nonNullable D.int4))

phraseGetSt :: Statement PhraseID (Maybe Phrase)
phraseGetSt = Statement sql encoder decoder True
  where
    sql = 
      "SELECT id :: int4, author_id :: int4, text :: text, created_at :: timestamptz, \
      \is_open :: bool, chosen_alt_id :: int4?, \
      \(SELECT COUNT(*) FROM alternatives WHERE phrase_id = $1 :: int4) :: int4, \
      \ spellcheck.data :: json \
      \FROM phrases \
      \JOIN spellcheck ON spellcheck.id = phrases.spellcheck_id \
      \WHERE id = $1 :: int4"
    encoder = contramap unPhraseId (E.param (E.nonNullable E.int4))
    decoder = D.rowMaybe $ Phrase
      <$> (PhraseID <$> D.column (D.nonNullable D.int4))
      <*> (UserID <$> D.column (D.nonNullable D.int4))
      <*> D.column (D.nonNullable D.text)
      <*> D.column (D.nonNullable D.timestamptz)
      <*> D.column (D.nonNullable D.bool)
      <*> (fmap AlternativeID <$> D.column (D.nullable D.int4))
       <*> D.column (D.nonNullable D.int4)
      -- <*> (SpellCheck <$> D.column (D.nonNullable D.json))

phraseListSt :: Statement (Bool, Maybe UserID) (Vector PhraseID)
phraseListSt = Statement sql encoder decoder True
  where
    sql = 
      "SELECT id :: int4 \
      \FROM phrases \
      \WHERE (NOT $1 :: bool OR is_open = $1 :: bool) \
      \AND ($2 IS NULL OR author_id = $2) \
      \ORDER BY created_at DESC"
    encoder = contramap sel1 (E.param (E.nonNullable E.bool))
           <> contramap (fmap unUserId . sel2) (E.param (E.nullable E.int4))
    decoder = D.rowVector (PhraseID <$> D.column (D.nonNullable D.int4))

phraseInsertSt :: Statement (UserID, Text, SpellCheckID) PhraseID
phraseInsertSt = Statement sql encoder decoder True
  where
    sql = 
      "INSERT INTO phrases (author_id, text, spellcheck_id) \
      \VALUES ($1 :: int4, $2 :: text, $3 :: int4) \
      \RETURNING id :: int4"
    encoder = contramap (unUserId . sel1) (E.param (E.nonNullable E.int4))
           <> contramap sel2 (E.param (E.nonNullable E.text))
           <> contramap (unSpellCheckId . sel3) (E.param (E.nonNullable E.int4))
    decoder = D.singleRow (PhraseID <$> D.column (D.nonNullable D.int4))

phraseSetChosenAltSt :: Statement (PhraseID, AlternativeID) ()
phraseSetChosenAltSt = Statement sql encoder decoder True
  where
    sql = 
      "UPDATE phrases \
      \SET chosen_alt_id = $2 :: int4, is_open = FALSE \
      \WHERE id = $1 :: int4"
    encoder = contramap (unPhraseId . sel1) (E.param (E.nonNullable E.int4))
           <> contramap (unAlternativeId . sel2) (E.param (E.nonNullable E.int4))
    decoder = D.noResult

alternativeGetSt :: Statement AlternativeID (Maybe Alternative)
alternativeGetSt = Statement sql encoder decoder True
  where
    sql = 
      "SELECT id :: int4, author_id :: int4, phrase_id :: int4, text :: text, created_at :: timestamptz, \
      \spellcheck.data :: json \
      \FROM alternatives \
      \JOIN spellcheck ON spellcheck.id = alternatives.spellcheck_id \
      \WHERE id = $1 :: int4"
    encoder = contramap unAlternativeId (E.param (E.nonNullable E.int4))
    decoder = D.rowMaybe $ Alternative
      <$> (AlternativeID <$> D.column (D.nonNullable D.int4))
      <*> (UserID <$> D.column (D.nonNullable D.int4))
      <*> (PhraseID <$> D.column (D.nonNullable D.int4))
      <*> D.column (D.nonNullable D.text)
      <*> D.column (D.nonNullable D.timestamptz)
      -- <*> (SpellCheck <$> D.column (D.nonNullable D.json))

alternativeListByPhraseSt :: Statement PhraseID (Vector AlternativeID)
alternativeListByPhraseSt = Statement sql encoder decoder True
  where
    sql = 
      "SELECT id :: int4 \
      \FROM alternatives \
      \WHERE phrase_id = $1 :: int4 \
      \ORDER BY created_at DESC"
    encoder = contramap unPhraseId (E.param (E.nonNullable E.int4))
    decoder = D.rowVector (AlternativeID <$> D.column (D.nonNullable D.int4))

alternativeListSt :: Statement (Maybe UserID) (Vector AlternativeID)
alternativeListSt = Statement sql encoder decoder True
  where
    sql = 
      "SELECT id :: int4 \
      \FROM alternatives \
      \WHERE $1 IS NULL OR author_id = $1 \
      \ORDER BY created_at DESC"
    encoder = contramap (fmap unUserId) (E.param (E.nullable E.int4))
    decoder = D.rowVector (AlternativeID <$> D.column (D.nonNullable D.int4))

alternativeInsertSt :: Statement (UserID, PhraseID, Text, SpellCheckID) AlternativeID
alternativeInsertSt = Statement sql encoder decoder True
  where
    sql = 
      "INSERT INTO alternatives (author_id, phrase_id, text, spellcheck_id) \
      \VALUES ($1 :: int4, $2 :: int4, $3 :: text, $4 :: int4) \
      \RETURNING id :: int4"
    encoder = contramap (unUserId . sel1) (E.param (E.nonNullable E.int4))
           <> contramap (unPhraseId . sel2) (E.param (E.nonNullable E.int4))
           <> contramap sel3 (E.param (E.nonNullable E.text))
           <> contramap (unSpellCheckId . sel4) (E.param (E.nonNullable E.int4))
    decoder = D.singleRow (AlternativeID <$> D.column (D.nonNullable D.int4))
  
spellCheckInsertSt :: Statement SpellCheck SpellCheckID
spellCheckInsertSt = Statement sql encoder decoder True
  where
    sql = 
      "INSERT INTO spellcheck (data) \
      \VALUES ($1 :: json) \
      \RETURNING id :: int4"
    encoder = contramap unSpellCheck (E.param (E.nonNullable E.json))
    decoder = D.singleRow (SpellCheckID <$> D.column (D.nonNullable D.int4))



    
