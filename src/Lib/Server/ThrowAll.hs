{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib.Server.ThrowAll (ThrowAll(throwAll)) where

import Control.Monad.Except (throwError)
import Servant ((:<|>)(..))
import Lib.App.Error (AppError)
import Lib.App (AppM)

class ThrowAll a where
    -- | By analogy with https://hackage.haskell.org/package/servant-auth-server-0.4.9.0/docs/src/Servant.Auth.Server.Internal.ThrowAll.html#ThrowAll
    throwAll :: AppError -> a

instance (ThrowAll a, ThrowAll b) => ThrowAll (a :<|> b) where
  throwAll e = throwAll e :<|> throwAll e

instance ThrowAll (AppM a) where
  throwAll = throwError

instance ThrowAll b => ThrowAll (a -> b) where
  throwAll e = const $ throwAll e