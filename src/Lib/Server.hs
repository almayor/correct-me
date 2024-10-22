module Lib.Server
        ( server
        , application
        ) where

import Data.Vector (Vector)
import Control.Monad (when)
import Servant hiding (BasicAuth)
import Servant.Auth.Server hiding (throwAll)

import Lib.App.Monad
import Lib.App.Error
import Lib.Server.ThrowAll
import Lib.Core.Types
import Lib.Core.Password
import Lib.Api
import Lib.Server.Auth (authenticate)
import Lib.Db

server :: ServerT API App
server = publicServer :<|> protectedServer

publicServer :: ServerT PublicAPI App
publicServer = registerH

protectedServer :: AuthResult User -> ServerT ProtectedAPI App
protectedServer (Authenticated u) = userH :<|> phraseH :<|> alternativeH
    where
    userH =
            listUsersH u
        :<|> getUserH u
        :<|> listPhrasesByUserH u
    phraseH =
            listPhrasesH u
        :<|> insertPhraseH u
        :<|> getPhraseH u
        :<|> listAlternativesByPhraseH u
        :<|> insertAlternativeH u
    alternativeH =
            listAlternatives u
        :<|> getAlternativeH u
        :<|> setAlternativeH u
protectedServer _ = throwAll NotAuthenticatedError

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


type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult User)

instance FromBasicAuthData User where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

application :: Env -> Application
application env = 
    let jwtCfg = defaultJWTSettings (envJWTKey env)
        authCfg = authenticate env
        ctx = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
        test = Proxy :: Proxy '[BasicAuthCfg, CookieSettings, JWTSettings]
        hoistedServer = hoistServerWithContext
            (Proxy @API)
            test
            (runAppAsHandler env)
            server
    in serveWithContext (Proxy @API) ctx hoistedServer
