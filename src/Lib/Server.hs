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

server :: ServerT API App
server = registerH :<|> accessH

accessH :: UserID -> App NoContent
accessH = undefined

registerH :: UserReq -> App EntryID
registerH (UserReq username password) = do
    exists <- execute userExistsSt username
    when exists userAlreadyExistsError
    pwdHash <- mkPasswordHash password
    execute userInsertSt (username, pwdHash)

application :: Env -> Application
application env = 
    let ctx = BasicAuthCheck (authenticate env) :. EmptyContext
        hoistedServer = hoistServerWithContext
            (Proxy @API)
            (Proxy @'[BasicAuthCheck UserID])
            (runAppAsHandler env)
            server
    in serveWithContext (Proxy @API) ctx hoistedServer
