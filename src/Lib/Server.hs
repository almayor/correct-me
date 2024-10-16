module Lib.Server
        ( server
        , application
        ) where

import Data.Vector (Vector)
import Control.Monad (when)
import Servant

import Lib.App.Monad
import Lib.App.Error
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
        :<|> listAlternativesH u
        :<|> insertAlternativeH u
    alternativeH u =
            getAlternativeH u
    publicH = registerH
    protectedH u = userH u :<|> phraseH u :<|> alternativeH u

registerH :: UserReq -> App LocPath
registerH (UserReq username password) = do
    exists <- execute userExistsSt username
    when exists userAlreadyExistsError
    pwdHash <- mkPasswordHash password
    userId <- execute userInsertSt (username, pwdHash)
    return $ userId2Loc userId

insertPhraseH :: User -> PhraseReq -> App LocPath
insertPhraseH (User { userId }) (PhraseReq { phraseReqText }) = do
    phraseId <- execute phraseInsertSt (userId, phraseReqText)
    return $ phraseId2Loc phraseId

insertAlternativeH :: User -> EntryID -> AlternativeReq -> App LocPath
insertAlternativeH (User { userId }) phraseId (AlternativeReq { altReqText }) = do
    altId <- execute alternativeInsertSt (userId, phraseId, altReqText)
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

listAlternativesH :: User -> EntryID -> App (Vector LocPath)
listAlternativesH _ phraseId = do
    entries <- execute alternativeListSt phraseId
    return $ alternativeId2Loc <$> entries

getUserH :: User -> UserID -> App User
getUserH _ userId = do
    entry <- execute userGetSt userId
    maybe notFoundError return entry

getPhraseH :: User -> EntryID -> App Phrase
getPhraseH _ phraseId = do
    entry <- execute phraseGetSt phraseId
    maybe notFoundError return entry

getAlternativeH :: User -> EntryID -> App Alternative
getAlternativeH _ altId = do
    entry <- execute alternativeGetSt altId
    maybe notFoundError return entry

application :: Env -> Application
application env = 
    let ctx = BasicAuthCheck (authenticate env) :. EmptyContext
        hoistedServer = hoistServerWithContext
            (Proxy @API)
            (Proxy @'[BasicAuthCheck User])
            (runAppAsHandler env)
            server
    in serveWithContext (Proxy @API) ctx hoistedServer
