module Lib.Config
    ( AppConfig(..)
    , loadConfig
    ) where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Environment (lookupEnv)
import Toml (TomlCodec, (.=))
import qualified Toml


data AppConfig = AppConfig
    { configAppDescription  :: !Text
    , configAppPort         :: !Int
    , configDbHost          :: !ByteString
    , configDbPort          :: !Int
    , configDbName          :: !ByteString
    , configDbUser          :: !ByteString
    , configDbPass          :: !ByteString
    }

configCodec :: TomlCodec AppConfig
configCodec = AppConfig
    <$> Toml.text "app.description" .= configAppDescription
    <*> Toml.int "app.port"         .= configAppPort
    <*> Toml.byteString "db.host"   .= configDbHost
    <*> Toml.int "db.port"          .= configDbPort
    <*> Toml.byteString "db.name"   .= configDbHost
    <*> Toml.byteString "db.user"   .= configDbUser
    <*> Toml.byteString "db.pass"   .= configDbPass

loadConfig :: IO AppConfig
loadConfig = do
    configFile <- fromMaybe "config.toml" <$> lookupEnv "CONFIG_FILE"
    Toml.decodeFile configCodec configFile
