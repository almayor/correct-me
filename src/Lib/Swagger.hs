module Lib.Swagger (
    applicationSwagger
    ) where

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
    & host              ?~ fromString ("localhost:" <> show (appPort config))

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

serverSwagger :: AppConfig -> Server SwaggerAPI
serverSwagger = swaggerSchemaUIServer . swaggerDoc

applicationSwagger :: AppConfig -> Application
applicationSwagger = serve (Proxy @SwaggerAPI) . serverSwagger
