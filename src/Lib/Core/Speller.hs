module Lib.Core.Speller where

import Control.Lens hiding ((.=))         -- For lens manipulation (used by wreq)
import Data.Aeson           -- For working with JSON
import qualified Data.ByteString.Lazy.Char8 as B
import Network.Wreq         -- The wreq library itself
import Control.Exception    -- For error handling
import Data.Text            -- To handle textual data in Haskell

import Lib.Core.Types (SpellCheck)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Network.URI

yandexSpeller :: (MonadIO m, MonadLogger m, Exception e) => URI -> Text -> m (Either e SpellCheck)
yandexSpeller uri text = do
    let postData = object ["text" .= text]
    let postOptions = defaults & header "Content-Type" .~ ["application/json"]
    $logInfo $ "Requesting spellcheck from " <> (pack . show) uri  <> " with data: " <> (pack . show) postData
    response <- liftIO . try $ postWith postOptions (show uri) postData
    case response of
        Left e -> return $ Left e
        Right r -> do
            let body = r ^. responseBody
            $logInfo $ "Received response: " <> (pack . B.unpack) body
            case decode body of
                Nothing -> do
                    $logError "Failed to parse response"
                    pure $ Left (userError "Failed to parse response")
                Just spellCheck -> return $ Right spellCheck
                -- Assuming the previous version had a different way of handling the response
                -- Here is a possible previous version of the code:

                -- Replace the current $SELECTION_PLACEHOLDER$ with the following:



   
