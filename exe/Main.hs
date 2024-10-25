import System.Exit
import System.IO
import Options.Applicative
import Data.Semigroup ((<>))  -- For combining options
import Control.Monad (when)   -- For the 'when' function
import Data.Maybe (isJust, fromJust) -- For 'isJust' and 'fromJust'
import Data.Text (unpack) -- For converting a string to Text

import Lib

data Options = Options
  { optConfig     :: FilePath
  , optPrintDocs  :: Bool
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
    <*> switch
        ( long "print-docs" 
        <> help "Print API docs" )

optsParserInfo :: AppConfig -> ParserInfo Options
optsParserInfo config = info (optionsParser <**> helper)
    ( fullDesc
  <> progDesc (unpack $ appDescription config) )

main :: IO ()
main = do
    defaultConfig <- loadConfig
    opts <- execParser (optsParserInfo defaultConfig)

    when (optPrintDocs opts) $ do
        putStrLn docsMarkdown
        exitSuccess

    config <- loadConfigFromFile (optConfig opts)
    hPutStrLn stderr $ "Running with config: " ++ show config
    initDb config
    runServer config
