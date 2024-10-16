module Lib.Server
        ( server
        , application
        ) where

import Control.Monad (when)
import Servant

import Lib.App.Monad
import Lib.App.Error
import Lib.Core.Password
import Lib.Api
import Lib.Server.Auth (authenticate)
import Lib.Db
import Data.Vector (Vector)
import Data.Aeson.KeyMap (insert)

server :: ServerT API App
server = let phrasesH user = listPhrasesH user :<|> insertPhraseH user
          in registerH :<|> phrasesH

-- phrasesH :: User -> ServerT PhrasesAPI App
-- phrasesH user = listPhrasesH user :<|> insertPhraseH user

registerH :: UserReq -> App EntryID
registerH (UserReq username password) = do
    exists <- execute userExistsSt username
    when exists userAlreadyExistsError
    pwdHash <- mkPasswordHash password
    execute userInsertSt (username, pwdHash)

listPhrasesH :: User -> Maybe UserID -> Bool -> App (Vector Phrase)
listPhrasesH _ authorId isOpen = do
    execute phraseGetAllSt (isOpen, authorId)

insertPhraseH :: User -> PhraseReq -> App EntryID
insertPhraseH (User { userId }) (PhraseReq { phraseReqText }) = do
    execute phrasesInsertSt (userId, phraseReqText)

application :: Env -> Application
application env = 
    let ctx = BasicAuthCheck (authenticate env) :. EmptyContext
        hoistedServer = hoistServerWithContext
            (Proxy @API)
            (Proxy @'[BasicAuthCheck UserID])
            (runAppAsHandler env)
            server
    in serveWithContext (Proxy @API) ctx hoistedServer
