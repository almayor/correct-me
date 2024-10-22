module Lib.Docs (apiDocs) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Servant
import Servant.Docs

import Data.Proxy (Proxy(Proxy))
import Lib.Api (AppAPI)

-- Define some sample data
-- instance ToSample String where
--   toSamples _ = [("Example response for hello", "Hello, world!"),
--          ("Example response for goodbye", "Goodbye, world!")]

-- Generate the documentation
apiDocs :: API
apiDocs = undefined
