module Lib.Core.Utils (modifyLabel) where

import Text.Casing (quietSnake)

modifyLabel :: String -> String -> String
modifyLabel prefix = quietSnake . drop (length prefix)
