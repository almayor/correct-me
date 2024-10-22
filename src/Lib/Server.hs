{-# OPTIONS_GHC -Wno-orphans #-}
module Lib.Server
        ( appServer
        , application
        ) where

import Servant hiding (BasicAuth)
import Servant.Auth.Server hiding (throwAll)

import Lib.App.Monad
import Lib.App.Error
import Lib.Types
import Lib.Api

import Lib.Server.Auth (authenticate)
import Lib.Server.Handlers
import Lib.Server.ThrowAll (throwAll)

import Lib.Docs (DocsAPI, docsServer)
import Lib.Swagger (SwaggerAPI, swaggerServer)

type FullAPI = AppAPI :<|> SwaggerAPI :<|> DocsAPI

publicAppServer :: ServerT PublicAppAPI AppM
publicAppServer = registerH

protectedAppServer :: AuthResult User -> ServerT ProtectedAppAPI AppM
protectedAppServer (Authenticated user) = userH user :<|> phraseH user :<|> alternativeH user
    where
    userH u =
            listUsersH u
        :<|> getUserH u
        :<|> listPhrasesByUserH u
    phraseH u =
            listPhrasesH u
        :<|> insertPhraseH u
        :<|> getPhraseH u
        :<|> listAlternativesByPhraseH u
        :<|> insertAlternativeH u
    alternativeH u =
            listAlternatives u
        :<|> getAlternativeH u
        :<|> chooseAlternativeH u
protectedAppServer _ = throwAll NotAuthenticatedError

appServer :: ServerT AppAPI AppM
appServer = publicAppServer :<|> protectedAppServer

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult User)

instance FromBasicAuthData User where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

application :: Env -> Application
application env = 
    let jwtCfg = defaultJWTSettings (envJWTKey env)
        authCfg = authenticate env
        ctx = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
        hoistedServer = hoistServerWithContext
            (Proxy @AppAPI)
            (Proxy @'[BasicAuthCfg, CookieSettings, JWTSettings])
            (runAppAsHandler env)
            appServer
        fullServer = hoistedServer :<|> swaggerServer (envConfig env) :<|> docsServer
    in serveWithContext (Proxy @FullAPI) ctx fullServer
