module Server where

import Control.Monad (when)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Wai.Handler.Warp (run)
import Servant

import Database
import Error
import Monad
import Password
import Types


server :: ServerT API App
server = register :<|> access

access :: UserID -> App NoContent
access = undefined

-- userExists username = do
--     pool <- asks envDbPool
--     let sess = statement username userExistsSt
--     result <- Pool.use pool sess
--     case result of
--         Left err -> throwError $ err500 { errBody = "Database error" }
--         Right exists -> return exists

-- authCheck :: App (BasicAuthCheck UserID)
-- authCheck = return $ BasicAuthCheck (liftIO check)

authenticate' :: BasicAuthData -> App (BasicAuthResult UserID)
authenticate' (BasicAuthData username password) = do
    user <- execute getUserByUsernameSt (T.decodeLatin1 username)
    case user of
        Nothing -> return NoSuchUser
        Just (UserDB { userId, userPwdHash }) ->
            if verifyPassword (T.decodeASCII password) userPwdHash
                then return $ Authorized userId
                else return BadPassword

authenticate :: Env -> BasicAuthData -> IO (BasicAuthResult UserID)
authenticate env d = runAppAsIO env (authenticate' d)

register :: UserReq -> App EntryID
register (UserReq username password) = do
    exists <- execute userExistsSt username
    when exists userAlreadyExistsError
    pwdHash <- mkPasswordHash password
    execute insertUserSt (username, pwdHash)

-- application :: Env -> Application
-- application env =
--     let api = Proxy @API
--         hoistedServer = 
--     in serveWithContext api ctx $ hoistServer api (runAppAsHandler env) server

main' :: IO ()
main' = do
    config <- loadConfig
    env <- mkEnv config

    let ctx = BasicAuthCheck (authenticate env) :. EmptyContext
        hoistedServer = hoistServerWithContext api_proxy auth_proxy (runAppAsHandler env) server
        app = serveWithContext api_proxy ctx hoistedServer

    run 8080 app

    where
        api_proxy = Proxy :: Proxy API
        auth_proxy = Proxy :: Proxy '[BasicAuthCheck UserID]


application :: Env -> Application
application env = 
    let ctx = BasicAuthCheck (authenticate env) :. EmptyContext
        hoistedServer = hoistServerWithContext
            (Proxy @API)
            (Proxy @'[BasicAuthCheck UserID])
            (runAppAsHandler env)
            server
    in serveWithContext (Proxy @API) ctx hoistedServer
    

runServer :: Env -> IO ()
runServer env = do
    let ctx = BasicAuthCheck (authenticate env) :. EmptyContext
        hoistedServer = hoistServerWithContext api_proxy auth_proxy (runAppAsHandler env) server
        app = serveWithContext api_proxy ctx hoistedServer

    run 8080 app

    where
        api_proxy = Proxy :: Proxy API
        auth_proxy = Proxy :: Proxy '[BasicAuthCheck UserID]