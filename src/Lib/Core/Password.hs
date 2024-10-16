module Lib.Core.Password where

import qualified Crypto.BCrypt as BCrypt

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Lib.App.Error

type PasswordHash = Text
type PasswordPlainText = Text

-- This has to be done in IO asy generating the salt requires RNG.
mkPasswordHashWithPolicy ::
    (CanFail m, MonadIO m)
    => BCrypt.HashingPolicy
    -> PasswordPlainText
    -> m PasswordHash
mkPasswordHashWithPolicy policy pwd = do
    let pwdBS = encodeUtf8 pwd
    hashBS <- liftIO $ BCrypt.hashPasswordUsingPolicy policy pwdBS
    case hashBS of
        Just hashBS' -> pure . decodeUtf8 $ hashBS'
        Nothing -> internalError "Failed to hash password"

-- | Generates the password hash with fast hashing policy.
mkPasswordHash ::
    (CanFail m, MonadIO m)
    => PasswordPlainText
    -> m PasswordHash
mkPasswordHash = mkPasswordHashWithPolicy BCrypt.fastBcryptHashingPolicy

-- | Verifies the password hash.
verifyPassword :: PasswordPlainText -> PasswordHash -> Bool
verifyPassword pwd hash =
    BCrypt.validatePassword (encodeUtf8 hash) (encodeUtf8 pwd)