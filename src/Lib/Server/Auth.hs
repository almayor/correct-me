module Lib.Server.Auth (authenticate) where


import qualified Data.Text.Encoding as T
import Servant

import Lib.Db
import Lib.App.Monad
import Lib.Core.Password
import Lib.Api

authenticate' :: BasicAuthData -> App (BasicAuthResult User)
authenticate' (BasicAuthData username password) = do
    result <- execute userGetByUsernameSt (T.decodeLatin1 username)
    case result of
        Nothing -> return NoSuchUser
        Just user@User { userPwdHash } ->
            if verifyPassword (T.decodeASCII password) userPwdHash
                then return $ Authorized user
                else return BadPassword

authenticate :: Env -> BasicAuthData -> IO (BasicAuthResult User)
authenticate env d = runAppAsIO env (authenticate' d)


