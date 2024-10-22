module Lib.Server.Auth (authenticate) where

import Servant.Auth.Server

import Lib.Db
import Lib.App.Monad
import Lib.Core.Password
import Lib.Core.Types
import Lib.Core.Username

authenticate' :: BasicAuthData -> App (AuthResult User)
authenticate' (BasicAuthData userNameBS passwordBS) = do
    let userName = bs2UserName userNameBS
    let password = bs2Password passwordBS
    result <- execute userGetByUsernameSt userName
    case result of
        Nothing -> return NoSuchUser
        Just user -> do
            hash <- execute userGetPasswordSt (userId user)
            if verifyPassword password hash
                then return $ Authenticated user
                else return BadPassword

authenticate :: Env -> BasicAuthData -> IO (AuthResult User)
authenticate env d = runAppAsIO env (authenticate' d)

