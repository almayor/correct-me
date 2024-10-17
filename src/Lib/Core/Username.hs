module Lib.Core.Username (bs2UserName) where

import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)

import Lib.Core.Types

bs2UserName :: ByteString -> UserName
bs2UserName = UserName . decodeUtf8