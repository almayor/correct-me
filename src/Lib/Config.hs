module Lib.Config
    ( AppConfig(..)
    , DbConfig(..)
    , loadConfig
    ) where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Environment (lookupEnv)
import Toml (TomlCodec, (.=))
import qualified Toml


data AppConfig = AppConfig
    { configDescription :: !Text
    , configDb          :: !DbConfig
    }

data DbConfig = DbConfig
    { configDbHost :: !ByteString
    , configDbPort :: !Int
    , configDbName :: !ByteString
    , configDbUser :: !ByteString
    , configDbPass :: !ByteString
    }

dbCodec :: TomlCodec DbConfig
dbCodec = DbConfig
    <$> Toml.byteString "host" .= configDbHost
    <*> Toml.int "port"        .= configDbPort
    <*> Toml.byteString "name" .= configDbHost
    <*> Toml.byteString "user" .= configDbUser
    <*> Toml.byteString "pass" .= configDbPass

configCodec :: TomlCodec AppConfig
configCodec = AppConfig
    <$> Toml.text "app.description" .= configDescription
    <*> Toml.table dbCodec "db" .= configDb

loadConfig :: IO AppConfig
loadConfig = do
    configFile <- fromMaybe "config.toml" <$> lookupEnv "CONFIG_FILE"
    Toml.decodeFile configCodec configFile
