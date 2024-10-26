{-# OPTIONS_GHC -Wno-orphans #-}
module Lib.Server.Handlers where

import Data.Vector (Vector)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Control.Monad (when, unless)
import Servant hiding (BasicAuth)
import Text.Regex.TDFA ((=~))

import Lib.App.Monad
import Lib.App.Error
import Lib.Types
import Lib.Core.Password
import Lib.Api
import Lib.Db

validateUsername :: UserName -> AppM ()
validateUsername (UserName userName) = do
    let l = length . unpack $ userName
    when (l < 5 || l > 20) $
        throwError $ InvalidContent "Username be between 5 and 20 characters long"
    when (userName =~ ("^[[:space:]]|[[:space:]]$" :: Text) :: Bool) $
        throwError $ InvalidContent "Username must not begin or end with a space"
    when (userName =~ ("[[:space:]]{2,}" :: Text) :: Bool) $
        throwError $ InvalidContent "Username must not contain consecutive spaces"
    when (userName =~ ("[^[:alnum:]_]" :: Text) :: Bool) $
        throwError $ InvalidContent "Username must contain only letters, digits, and underscores"

validatePassword :: PasswordPlain -> AppM ()
validatePassword (PasswordPlain password) = do
    let l = length . unpack $ password
    when (l < 8 || l > 25) $
        throwError $ InvalidContent "Password must be between 8 and 25 characters long"

validatePhrase :: Text -> AppM ()
validatePhrase phrase = do
    when (T.null phrase) $ throwError $ InvalidContent "Phrase must not be empty"

validateAlternative :: Text -> AppM ()
validateAlternative alt = do
    when (T.null alt) $ throwError $ InvalidContent "Alternative must not be empty"

registerH :: UserReq -> AppM LocPath
registerH (UserReq userName password) = do
    exists <- execute userExistsByNameSt userName
    when exists $ throwError UserAlreadyExistsError
    validateUsername userName
    validatePassword password
    pwdHash <- mkPasswordHash password
    userId <- execute userInsertSt (userName, pwdHash)
    return $ userId2Loc userId

insertPhraseH :: User -> PhraseReq -> AppM LocPath
insertPhraseH (User { userId }) (PhraseReq { phraseReqText }) = do
    validatePhrase phraseReqText
    spellCheck <- runSpeller phraseReqText
    spellCheckId <- execute spellCheckInsertSt spellCheck
    phraseId <- execute phraseInsertSt (userId, phraseReqText, spellCheckId)
    return $ phraseId2Loc phraseId

insertAlternativeH :: User -> PhraseID -> AlternativeReq -> AppM LocPath
insertAlternativeH (User { userId }) phraseId (AlternativeReq { altReqText }) = do
    phrase <- execute phraseGetSt phraseId >>= maybe (throwError NotFoundError) return
    unless (phraseIsOpen phrase) $ throwError PhraseAlreadyClosedError
    validateAlternative altReqText
    spellCheck <- runSpeller altReqText
    spellCheckId <- execute spellCheckInsertSt spellCheck
    altId <- execute alternativeInsertSt (userId, phraseId, altReqText, spellCheckId)
    return $ alternativeId2Loc altId

listUsersH :: User -> AppM (Vector LocPath)
listUsersH _ = do
    entries <- execute usersListSt ()
    return $ userId2Loc <$> entries

listPhrasesByUserH :: User -> UserID -> Bool -> AppM (Vector LocPath)
listPhrasesByUserH _ authorId isOpen = do
    entries <- execute phraseListSt (isOpen, Just authorId)
    return $ phraseId2Loc <$> entries

listPhrasesH :: User -> Bool -> AppM (Vector LocPath)
listPhrasesH _ isOpen = do
    entries <- execute phraseListSt (isOpen, Nothing)
    return $ phraseId2Loc <$> entries

listAlternativesByPhraseH :: User -> PhraseID -> AppM (Vector LocPath)
listAlternativesByPhraseH _ phraseId = do
    entries <- execute alternativeListByPhraseSt phraseId
    return $ alternativeId2Loc <$> entries

listAlternatives :: User -> Maybe UserID -> AppM (Vector LocPath)
listAlternatives _ authorId = do
    entries <- execute alternativeListSt authorId
    return $ alternativeId2Loc <$> entries

getUserH :: User -> UserID -> AppM User
getUserH _ userId = do
    entry <- execute userGetSt userId
    maybe (throwError NotFoundError) return entry

getPhraseH :: User -> PhraseID -> AppM Phrase
getPhraseH _ phraseId = do
    entry <- execute phraseGetSt phraseId
    maybe (throwError NotFoundError) return entry

getAlternativeH :: User -> AlternativeID -> AppM Alternative
getAlternativeH _ altId = do
    entry <- execute alternativeGetSt altId
    maybe (throwError NotFoundError) return entry

chooseAlternativeH :: User -> AlternativeID -> AppM LocPath
chooseAlternativeH (User { userId }) altId = do
    alt <- execute alternativeGetSt altId >>= maybe (throwError NotFoundError) return
    phrase <- execute phraseGetSt (altPhraseId alt) >>= maybe (throwError $ InternalError "Inconsistent database") return
    unless (phraseIsOpen phrase) $ throwError PhraseAlreadyClosedError
    when (phraseAuthorId phrase /= userId) $ throwError NotTheAuthorError
    execute phraseSetChosenAltSt (altPhraseId alt, altId)
    return $ phraseId2Loc (altPhraseId alt)
