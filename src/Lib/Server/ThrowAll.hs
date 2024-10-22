{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib.Server.ThrowAll where

import Control.Monad.Except (MonadError, throwError)
import Servant ((:<|>)(..))
import Lib.App.Error (AppError)
import Lib.App (App)

class ThrowAll a where
    -- | By analogy with https://hackage.haskell.org/package/servant-auth-server-0.4.9.0/docs/src/Servant.Auth.Server.Internal.ThrowAll.html#ThrowAll
    throwAll :: AppError -> a

instance (ThrowAll a, ThrowAll b) => ThrowAll (a :<|> b) where
  throwAll e = throwAll e :<|> throwAll e

instance ThrowAll (App a) where
  throwAll = throwError

instance ThrowAll b => ThrowAll (a -> b) where
  throwAll e = const $ throwAll e