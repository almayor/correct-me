module Lib.Server.Auth (authenticate) where


import qualified Data.Text.Encoding as T
import Servant

import Lib.Db
import Lib.App.Monad
import Lib.Core.Password
import Lib.Api

authenticate' :: BasicAuthData -> App (BasicAuthResult UserID)
authenticate' (BasicAuthData username password) = do
    user <- execute userGetByUsernameSt (T.decodeLatin1 username)
    case user of
        Nothing -> return NoSuchUser
        Just (UserDB { userId, userPwdHash }) ->
            if verifyPassword (T.decodeASCII password) userPwdHash
                then return $ Authorized userId
                else return BadPassword

authenticate :: Env -> BasicAuthData -> IO (BasicAuthResult UserID)
authenticate env d = runAppAsIO env (authenticate' d)


