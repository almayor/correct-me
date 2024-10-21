module Lib.Server
        ( server
        , application
        ) where

import Data.Vector (Vector)
import Control.Monad (when)
import Servant

import Lib.App.Monad
import Lib.App.Error
import Lib.Core.Types
import Lib.Core.Password
import Lib.Api
import Lib.Server.Auth (authenticate)
import Lib.Db

server :: ServerT API App
server = publicH :<|> protectedH
    where
    userH u =
            listUsersH u
        :<|> getUserH u
        :<|> listPhrasesByUserH u
    phraseH u =
            listPhrasesH u
        :<|> insertPhraseH u
        :<|> getPhraseH u
        :<|> listAlternativesByPhraseH u
        :<|> insertAlternativeH u
    alternativeH u =
            listAlternatives u
        :<|> getAlternativeH u
        :<|> setAlternativeH u
    publicH = registerH
    protectedH u = userH u :<|> phraseH u :<|> alternativeH u

registerH :: UserReq -> App LocPath
registerH (UserReq userName password) = do
    exists <- execute userExistsByNameSt userName
    when exists $ throwError UserAlreadyExistsError
    pwdHash <- mkPasswordHash password
    userId <- execute userInsertSt (userName, pwdHash)
    return $ userId2Loc userId

insertPhraseH :: User -> PhraseReq -> App LocPath
insertPhraseH (User { userId }) (PhraseReq { phraseReqText }) = do
    spellCheck <- runSpeller phraseReqText
    spellCheckId <- execute spellCheckInsertSt spellCheck
    phraseId <- execute phraseInsertSt (userId, phraseReqText, spellCheckId)
    return $ phraseId2Loc phraseId

insertAlternativeH :: User -> PhraseID -> AlternativeReq -> App LocPath
insertAlternativeH (User { userId }) phraseId (AlternativeReq { altReqText }) = do
    spellCheck <- runSpeller altReqText
    spellCheckId <- execute spellCheckInsertSt spellCheck
    altId <- execute alternativeInsertSt (userId, phraseId, altReqText, spellCheckId)
    return $ alternativeId2Loc altId

listUsersH :: User -> App (Vector LocPath)
listUsersH _ = do
    entries <- execute usersListSt ()
    return $ userId2Loc <$> entries

listPhrasesByUserH :: User -> UserID -> Bool -> App (Vector LocPath)
listPhrasesByUserH _ authorId isOpen = do
    entries <- execute phraseListSt (isOpen, Just authorId)
    return $ phraseId2Loc <$> entries

listPhrasesH :: User -> Bool -> App (Vector LocPath)
listPhrasesH _ isOpen = do
    entries <- execute phraseListSt (isOpen, Nothing)
    return $ phraseId2Loc <$> entries

listAlternativesByPhraseH :: User -> PhraseID -> App (Vector LocPath)
listAlternativesByPhraseH _ phraseId = do
    entries <- execute alternativeListByPhraseSt phraseId
    return $ alternativeId2Loc <$> entries

listAlternatives :: User -> Maybe UserID -> App (Vector LocPath)
listAlternatives _ authorId = do
    entries <- execute alternativeListSt authorId
    return $ alternativeId2Loc <$> entries

getUserH :: User -> UserID -> App User
getUserH _ userId = do
    entry <- execute userGetSt userId
    maybe (throwError NotFoundError) return entry

getPhraseH :: User -> PhraseID -> App Phrase
getPhraseH _ phraseId = do
    entry <- execute phraseGetSt phraseId
    maybe (throwError NotFoundError) return entry

getAlternativeH :: User -> AlternativeID -> App Alternative
getAlternativeH _ altId = do
    entry <- execute alternativeGetSt altId
    maybe (throwError NotFoundError) return entry

setAlternativeH :: User -> AlternativeID -> App LocPath
setAlternativeH (User { userId }) altId = do
    alt <- execute alternativeGetSt altId >>= maybe (throwError NotFoundError) return
    phrase <- execute phraseGetSt (altPhraseId alt) >>= maybe (throwError InconsistentDataError) return
    when (phraseAuthorId phrase /= userId) $ throwError NotTheAuthorError
    execute phraseSetChosenAltSt (altPhraseId alt, altId)
    return $ phraseId2Loc (altPhraseId alt)

application :: Env -> Application
application env = 
    let ctx = BasicAuthCheck (authenticate env) :. EmptyContext
        hoistedServer = hoistServerWithContext
            (Proxy @API)
            (Proxy @'[BasicAuthCheck User])
            (runAppAsHandler env)
            server
    in serveWithContext (Proxy @API) ctx hoistedServer
