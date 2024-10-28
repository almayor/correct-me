import System.Exit
import System.IO
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (when)
import Data.Text (unpack)

import Lib (loadConfigFromFile, runServer, initDb, docsMarkdown)

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
        <> showDefault
        )
    <*> switch
        ( long "print-docs" 
        <> help "Print API docs"
        )

optsParserInfo :: ParserInfo Options
optsParserInfo = info (optionsParser <**> helper)
    ( fullDesc
    <> progDesc "CorrectMe: A mini-service for reviewing message phrasing"
    )

main :: IO ()
main = do
    opts <- execParser optsParserInfo

    when (optPrintDocs opts) $ do
        putStrLn docsMarkdown
        exitSuccess

    config <- loadConfigFromFile (optConfig opts)
    hPutStrLn stderr $ "Running with config: " ++ show config
    initDb config
    runServer config
