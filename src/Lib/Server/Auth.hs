module Lib.Server.Auth (authenticate) where

import Servant

import Lib.Db
import Lib.App.Monad
import Lib.Core.Password
import Lib.Core.Types
import Lib.Core.Username

authenticate' :: BasicAuthData -> App (BasicAuthResult User)
authenticate' (BasicAuthData usernameBS passwordBS) = do
    let userName = bs2UserName usernameBS
    let password = bs2Password passwordBS
    result <- execute userGetByUsernameSt userName
    case result of
        Nothing -> return NoSuchUser
        Just user -> do
            hash <- execute userGetPasswordSt (userId user)
            if verifyPassword password hash
                then return $ Authorized user
                else return BadPassword

authenticate :: Env -> BasicAuthData -> IO (BasicAuthResult User)
authenticate env d = runAppAsIO env (authenticate' d)


