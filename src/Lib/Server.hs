{-# OPTIONS_GHC -Wno-orphans #-}
module Lib.Server
        ( appServer
        , application
        ) where

import Data.Vector (Vector)
import Control.Monad (when)
import Servant hiding (BasicAuth)
import Servant.Auth.Server hiding (throwAll)

import Lib.App.Monad
import Lib.App.Error
import Lib.Server.ThrowAll
import Lib.Types
import Lib.Core.Password
import Lib.Api
import Lib.Server.Auth (authenticate)
import Lib.Db

appServer :: ServerT AppAPI AppM
appServer = publicAppServer :<|> protectedAppServer

publicAppServer :: ServerT PublicAppAPI AppM
publicAppServer = registerH

protectedAppServer :: AuthResult User -> ServerT ProtectedAppAPI AppM
protectedAppServer (Authenticated user) = userH user :<|> phraseH user :<|> alternativeH user
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
        :<|> chooseAlternativeH u
protectedAppServer _ = throwAll NotAuthenticatedError

registerH :: UserReq -> AppM LocPath
registerH (UserReq userName password) = do
    exists <- execute userExistsByNameSt userName
    when exists $ throwError UserAlreadyExistsError
    pwdHash <- mkPasswordHash password
    userId <- execute userInsertSt (userName, pwdHash)
    return $ userId2Loc userId

insertPhraseH :: User -> PhraseReq -> AppM LocPath
insertPhraseH (User { userId }) (PhraseReq { phraseReqText }) = do
    spellCheck <- runSpeller phraseReqText
    spellCheckId <- execute spellCheckInsertSt spellCheck
    phraseId <- execute phraseInsertSt (userId, phraseReqText, spellCheckId)
    return $ phraseId2Loc phraseId

insertAlternativeH :: User -> PhraseID -> AlternativeReq -> AppM LocPath
insertAlternativeH (User { userId }) phraseId (AlternativeReq { altReqText }) = do
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
    phrase <- execute phraseGetSt (altPhraseId alt) >>= maybe (throwError InconsistentDataError) return
    when (phraseAuthorId phrase /= userId) $ throwError NotTheAuthorError
    execute phraseSetChosenAltSt (altPhraseId alt, altId)
    return $ phraseId2Loc (altPhraseId alt)

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult User)

instance FromBasicAuthData User where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

application :: Env -> Application
application env = 
    let jwtCfg = defaultJWTSettings (envJWTKey env)
        authCfg = authenticate env
        ctx = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
        hoistedServer = hoistServerWithContext
            (Proxy @AppAPI)
            (Proxy @'[BasicAuthCfg, CookieSettings, JWTSettings])
            (runAppAsHandler env)
            appServer
    in serveWithContext (Proxy @AppAPI) ctx hoistedServer
