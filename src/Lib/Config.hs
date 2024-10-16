module Lib.Config
    ( AppConfig(..)
    , loadConfig
    ) where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word16)
import System.Environment (lookupEnv)
import Toml (TomlCodec, (.=))
import qualified Toml

data AppConfig = AppConfig
    { configDescription :: !Text
    , configDbPort      :: !Word16
    , configDbHost      :: !ByteString
    , configDbName      :: !ByteString
    , configDbUser      :: !ByteString
    , configDbPass      :: !ByteString
    }

configCodec :: TomlCodec AppConfig
configCodec = AppConfig
    <$> Toml.text "app.description" .= configDescription
    <*> Toml.read "db.port" .= configDbPort
    <*> Toml.byteString "db.host" .= configDbHost
    <*> Toml.byteString "db.name" .= configDbName
    <*> Toml.byteString "db.user" .= configDbUser
    <*> Toml.byteString "db.pass" .= configDbPass

loadConfig :: IO AppConfig
loadConfig = do
    configFile <- fromMaybe "config.toml" <$> lookupEnv "CONFIG_FILE"
    Toml.decodeFile configCodec configFile
