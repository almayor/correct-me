module Lib.Swagger (
    swaggerServer,
    SwaggerAPI,
    ) where

import qualified Data.ByteString.Char8 as BS (pack, unpack)
import Data.Swagger
import Data.String (IsString(fromString))
import Control.Lens

import Servant.Swagger
import Servant.Swagger.UI
import Servant

import Lib.Api
import Lib.Config

swaggerDoc :: AppConfig -> Swagger
swaggerDoc config = toSwagger (Proxy @AppAPI)
    & info.title        .~ appName config
    & info.version      .~ appVersion config
    & info.description  ?~ appDescription config
    & host              ?~ fromString (BS.unpack (appHost config) <> ":" <> show (appPort config))

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerServer :: AppConfig -> Server SwaggerAPI
swaggerServer = swaggerSchemaUIServer . swaggerDoc
