module Lib.Swagger where

import Servant.Swagger
import Data.Swagger
import Data.Proxy

import Lib.Api
import Control.Lens

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy @API)
    & info.title        .~ "User API"
    & info.version      .~ "1.0"
    & info.description  ?~ "This is an API for the Users service"
    & info.license      ?~ "MIT"
    & host              ?~ "example.com"

