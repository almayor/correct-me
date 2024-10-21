module Lib.Core.Speller
    ( externalSpeller
    , mockSpeller
    ) where
    
import Control.Exception    -- For error handling
import Control.Lens hiding ((.=))         -- For lens manipulation (used by wreq)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Data.Aeson           -- For working with JSON
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text            -- To handle textual data in Haskell
import Network.URI
import Network.Wreq         -- The wreq library itself

import Lib.App.Error
import Lib.Core.Types (SpellCheck(..))

-- | Spellcheck the given text using Yandex Speller API.
externalSpeller :: (MonadIO m, CanFail m) => URI -> Text -> m SpellCheck
yandexSpeller uri text = do
    let postData = "text" := text
    $logInfo $ "Requesting spellcheck from " <> (pack . show) uri  <> " with data: " <> (pack . show) postData
    response <- liftIO . try @IOException $ postWith (defaults & header "Content-Type" .~ ["application/x-www-form-urlencoded"]) (show uri) postData
    case response of
        Left e -> handleError e
        Right r -> handleResponse r
  where
    handleError e = do
        $logError $ "Failed to send request: " <> (pack . show) e
        throwError $ ExternalServiceError ("Failed to send request: " <> show e)
    
    handleResponse r = do
        let body = r ^. responseBody
        $logInfo $ "Received response: " <> (pack . B.unpack) body
        case decode body of
            Nothing -> do
                $logError "Failed to parse response"
                throwError $ InternalError "Failed to parse response"
            Just spellCheck -> return spellCheck

-- | Mock spellchecker that always returns empty list of errors.
mockSpeller :: (MonadIO m, CanFail m) => Text -> m SpellCheck
mockSpeller _ = do
    $logInfo "Using mock spellchecker"
    return $ SpellCheck Null




