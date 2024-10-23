module Lib.Config
    ( AppConfig(..)
    , loadConfig
    ) where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Toml (TomlCodec, (.=))
import qualified Toml
import Network.URI (URI, parseURI)
import Data.Text (Text, pack, unpack)


data AppConfig = AppConfig
    { appName         :: !Text
    , appDescription  :: !Text
    , appPort         :: !Int
    , appVersion      :: !Text
    , dbHost          :: !ByteString
    , dbPort          :: !Int
    , dbName          :: !ByteString
    , dbUser          :: !ByteString
    , dbPass          :: !ByteString
    , spellerEnabled  :: !Bool
    , spellerUri      :: !URI
    }

-- Define the TomlBiMap for URI
uriCodec :: Toml.Key -> Toml.TomlCodec URI
uriCodec = Toml.textBy bwd fwd
    where
        bwd = pack . show
        fwd = maybe (Left "Invalid URI") Right . parseURI . unpack

configCodec :: TomlCodec AppConfig
configCodec = AppConfig
    <$> Toml.text "app.title"       .= appName
    <*> Toml.text "app.description" .= appDescription
    <*> Toml.int "app.port"         .= appPort
    <*> Toml.text "app.version"     .= appVersion
    <*> Toml.byteString "db.host"   .= dbHost
    <*> Toml.int "db.port"          .= dbPort
    <*> Toml.byteString "db.name"   .= dbName
    <*> Toml.byteString "db.user"   .= dbUser
    <*> Toml.byteString "db.pass"   .= dbPass
    <*> Toml.bool "speller.enabled" .= spellerEnabled
    <*> uriCodec "speller.url"      .= spellerUri

loadConfig :: IO AppConfig
loadConfig = do
    configFile <- fromMaybe "config.toml" <$> lookupEnv "CONFIG_FILE"
    Toml.decodeFile configCodec configFile
