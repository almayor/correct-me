module Lib.Core.Password
    (bs2Password, mkPasswordHash, verifyPassword)
    where

import qualified Crypto.BCrypt as BCrypt

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString (ByteString)

import Lib.Core.Types
import Lib.App.Error

bs2Password :: ByteString -> PasswordPlain
bs2Password = PasswordPlain . decodeUtf8

-- This has to be done in IO as generating the salt requires RNG.
mkPasswordHashWithPolicy ::
    (CanFail m, MonadIO m)
    => BCrypt.HashingPolicy
    -> PasswordPlain
    -> m PasswordHash
mkPasswordHashWithPolicy policy (PasswordPlain pwd) = do
    let pwdBS = encodeUtf8 pwd
    hashBS <- liftIO $ BCrypt.hashPasswordUsingPolicy policy pwdBS
    case hashBS of
        Just hashBS' -> pure . PasswordHash . decodeUtf8 $ hashBS'
        Nothing -> internalError "Failed to hash password"

-- | Generates the password hash with fast hashing policy.
mkPasswordHash ::
    (CanFail m, MonadIO m)
    => PasswordPlain
    -> m PasswordHash
mkPasswordHash = mkPasswordHashWithPolicy BCrypt.fastBcryptHashingPolicy

-- | Verifies the password hash.
verifyPassword :: PasswordPlain -> PasswordHash -> Bool
verifyPassword (PasswordPlain pwd) (PasswordHash hash) =
    BCrypt.validatePassword (encodeUtf8 hash) (encodeUtf8 pwd)