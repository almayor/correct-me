module API (API, api) where
import Servant
    ( Proxy(..),
      type (:<|>),
      Capture,
      JSON,
      NoContent,
      QueryFlag,
      QueryParam,
      ReqBody,
      type (:>),
      Get,
      Post,
      Put )

import Types
import Data.Text

type RegisterAPI = "register" :> ReqBody '[JSON] UserReq :> Post '[JSON] NoContent

type PhraseAPI = "phrases" :> (
         QueryParam "author_id" Int :> QueryFlag "open" :> Get '[JSON] [Phrase]
    :<|> ReqBody '[JSON] PhraseReq :> Post '[JSON] NoContent
    :<|> Capture "id" Int :> "alternatives" :> (
             Get '[JSON] [Alternative]
        :<|> ReqBody '[JSON] AlternativeReq :> Post '[JSON] NoContent
        :<|> Capture "id" Int :> "approve" :> Put '[JSON] NoContent
        )
    )

type API = RegisterAPI

api :: Proxy API
api = Proxy

