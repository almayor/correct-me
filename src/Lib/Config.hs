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
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.String (fromString)

data AppConfig = AppConfig
    { appName         :: !Text
    , appDescription  :: !Text
    , appVersion      :: !Text
    , appHost         :: !ByteString
    , appPort         :: !Int
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
        bwd = T.pack . show
        fwd = maybe (Left "Invalid URI") Right . parseURI . T.unpack

configCodec :: TomlCodec AppConfig
configCodec = AppConfig
    <$> Toml.text "app.title"       .= appName
    <*> Toml.text "app.description" .= appDescription
    <*> Toml.text "app.version"     .= appVersion
    <*> Toml.byteString "app.host"  .= appHost
    <*> Toml.int "app.port"         .= appPort
    <*> Toml.byteString "db.host"   .= dbHost
    <*> Toml.int "db.port"          .= dbPort
    <*> Toml.byteString "db.name"   .= dbName
    <*> Toml.byteString "db.user"   .= dbUser
    <*> Toml.byteString "db.pass"   .= dbPass
    <*> Toml.bool "speller.enabled" .= spellerEnabled
    <*> uriCodec "speller.url"      .= spellerUri

-- Looks up environmental variable or returns a default value
overrideInt :: String -> Int -> IO Int
overrideInt envKey defaultValue = do
    envValue <- lookupEnv envKey
    return $ maybe defaultValue read envValue

-- Looks up environmental variable or returns a default value
overrideByteString :: String -> ByteString -> IO ByteString
overrideByteString envKey defaultValue = do
    envValue <- lookupEnv envKey
    return $ maybe defaultValue fromString envValue

-- Looks up environmental variable or returns a default value
overrideBool :: String -> Bool -> IO Bool
overrideBool envKey defaultValue = do
    envValue <- lookupEnv envKey
    return $ maybe defaultValue parseBool envValue
  where
    parseBool "true"  = True
    parseBool "false" = False
    parseBool _       = defaultValue

-- Looks up environmental variable or returns a default value
overrideURI :: String -> URI -> IO URI
overrideURI envKey defaultValue = do
    envValue <- lookupEnv envKey
    return $ maybe defaultValue (fromMaybe defaultValue . parseURI) envValue

loadConfig :: IO AppConfig
loadConfig = do
    configFile <- fromMaybe "config.toml" <$> lookupEnv "CONFIG_FILE"
    config <- Toml.decodeFile configCodec configFile

    -- Override with environment variables if present
    appHost <- overrideByteString "CORRECTME_APP_HOST" (appHost config)
    appPort <- overrideInt "CORRECTME_APP_PORT" (appPort config)
    dbHost <- overrideByteString "CORRECTME_DB_HOST" (dbHost config)
    dbPort <- overrideInt "CORRECTME_DB_PORT" (dbPort config)
    dbName <- overrideByteString "CORRECTME_DB_NAME" (dbName config)
    dbUser <- overrideByteString "CORRECTME_DB_USER" (dbUser config)
    dbPass <- overrideByteString "CORRECTME_DB_PASS" (dbPass config)
    spellerEnabled <- overrideBool "CORRECTME_SPELLER_ENABLED" (spellerEnabled config)
    spellerUri <- overrideURI "CORRECTME_SPELLER_URL" (spellerUri config)

    return config
        { appHost = appHost
        , appPort = appPort
        , dbHost = dbHost
        , dbPort = dbPort
        , dbName = dbName
        , dbUser = dbUser
        , dbPass = dbPass
        , spellerEnabled = spellerEnabled
        , spellerUri = spellerUri
        }
