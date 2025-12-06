{-# LANGUAGE OverloadedStrings #-}

module CLI
  ( CLIOptions(..)
  , Mode(..)
  , parseCLI
  ) where

import Options.Applicative
import Data.Text (Text)
import qualified Data.Text as T

-- | CLI execution modes
data Mode
  = ParseMode Text      -- --parse with TSL string
  | REPLMode (Maybe Text)  -- REPL mode with optional initial state
  deriving (Show, Eq)

-- | CLI options
data CLIOptions = CLIOptions
  { cliMode :: Mode
  } deriving (Show, Eq)

-- | Parser for CLI options
parseCLI :: IO CLIOptions
parseCLI = execParser opts
  where
    opts = info (cliParser <**> helper)
      ( fullDesc
     <> progDesc "TFT-DSL: A DSL for encoding TFT game states"
     <> header "tft-dsl - TeamFight Tactics Domain-Specific Language" )

-- | Main CLI parser
cliParser :: Parser CLIOptions
cliParser = CLIOptions <$> modeParser

-- | Mode parser (parse mode or REPL mode)
modeParser :: Parser Mode
modeParser = parseModeParser <|> replModeParser

-- | Parse mode: --parse <tsl-string>
parseModeParser :: Parser Mode
parseModeParser = ParseMode . T.pack <$> strOption
  ( long "parse"
 <> metavar "TSL_STRING"
 <> help "Parse and display a TSL string" )

-- | REPL mode: default mode with optional initial state
replModeParser :: Parser Mode
replModeParser = REPLMode . fmap T.pack <$> optional (argument str
  ( metavar "[TSL_STRING]"
 <> help "Optional initial game state for REPL" ))
