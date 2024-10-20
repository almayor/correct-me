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
    { configAppDescription  :: !Text
    , configAppPort         :: !Int
    , configDbHost          :: !ByteString
    , configDbPort          :: !Int
    , configDbName          :: !ByteString
    , configDbUser          :: !ByteString
    , configDbPass          :: !ByteString
    , configSpellerEnabled  :: !Bool
    , configSpellerUrl      :: !URI
    }

-- Define the TomlBiMap for URI
uriCodec :: Toml.Key -> Toml.TomlCodec URI
uriCodec = Toml.textBy bwd fwd
    where
        bwd = pack . show
        fwd = maybe (Left "Invalid URI") Right . parseURI . unpack

configCodec :: TomlCodec AppConfig
configCodec = AppConfig
    <$> Toml.text "app.description" .= configAppDescription
    <*> Toml.int "app.port"         .= configAppPort
    <*> Toml.byteString "db.host"   .= configDbHost
    <*> Toml.int "db.port"          .= configDbPort
    <*> Toml.byteString "db.name"   .= configDbHost
    <*> Toml.byteString "db.user"   .= configDbUser
    <*> Toml.byteString "db.pass"   .= configDbPass
    <*> Toml.bool "speller.enabled" .= configSpellerEnabled
    <*> uriCodec "speller.url"      .= configSpellerUrl

loadConfig :: IO AppConfig
loadConfig = do
    configFile <- fromMaybe "config.toml" <$> lookupEnv "CONFIG_FILE"
    Toml.decodeFile configCodec configFile
