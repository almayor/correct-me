module Server where

import Servant
import Lib
import Database
import Types
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Reader (asks)
import Control.Monad (when)

import Error
import qualified Data.Text.Encoding as T
import Password
import Network.Wai.Handler.Warp (run)


server :: ServerT API App
server = register :<|> test-- :<|> access

test :: App NoContent
test = return NoContent

-- access :: UserID -> App NoContent
-- access = undefined

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
    execute insertUserSt (username, password)


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
        api = Proxy @API
        -- app = serveWithContext api ctx $ (hoistServerWithContext api (runAppAsHandler env) server)
        app = serve api $ hoistServer api (runAppAsHandler env) server

    run 8080 app


