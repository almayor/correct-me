import System.Exit
import Options.Applicative
import Data.Semigroup ((<>))  -- For combining options
import Control.Monad (when)   -- For the 'when' function
import Data.Maybe (isJust, fromJust) -- For 'isJust' and 'fromJust'
import Data.Text (unpack) -- For converting a string to Text

import Lib

data Options = Options
  { optConfig   :: FilePath
  , optDocsDest :: Maybe FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption
        ( long "config"
        <> short 'f'
        <> metavar "CONFIG"
        <> value "config.toml"
        <> help "Path to the configuration file"
        <> showDefault )
    <*> ( optional . strOption )
        ( long "output-docs" 
        <> metavar "PATH"
        <> help "Write API docs (markdown) to PATH and exit (optional)" )

optsParserInfo :: AppConfig -> ParserInfo Options
optsParserInfo config = info (optionsParser <**> helper)
    ( fullDesc
  <> progDesc (unpack $ appDescription config) )

main :: IO ()
main = do
    defaultConfig <- loadConfig
    opts <- execParser (optsParserInfo defaultConfig)

    when (isJust $ optDocsDest opts) $ do
        writeDocs . fromJust $ optDocsDest opts
        exitSuccess

    config <- loadConfigFromFile (optConfig opts)
    initDb config
    runServer config
