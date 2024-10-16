module Lib.Api where

import Servant
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.TH (deriveJSON, Options (fieldLabelModifier), defaultOptions)
import Text.Casing (quietSnake)
import Data.Int (Int32)
import Data.Time (UTCTime)

type EntryID = Int32
type UserID = EntryID

data UserReq = UserReq
    { userReqUsername :: Text
    , userReqPassword :: Text
    } deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = quietSnake . drop 7} ''UserReq)

type RegisterAPI = "register" :> ReqBody '[JSON] UserReq :> Post '[JSON] UserID
type AccessAPI = BasicAuth "access" UserID :> GetNoContent
type API = RegisterAPI :<|> AccessAPI
