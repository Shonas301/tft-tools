{-# LANGUAGE OverloadedStrings #-}

module Main where

import CLI
import REPL
import Types
import Parser (parseGameState)
import DataLoader
import System.Exit (exitFailure)
import qualified Data.Text as T
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.IO (openFile, IOMode(..), hClose, hPutStrLn)

main :: IO ()
main = do
  options <- parseCLI

  -- Load game data from CSV files
  dataResult <- loadGameData
    "../tft-data/set_16_champions.csv"
    "../tft-data/set_16_items.csv"
    "../tft-data/set_16_augments.csv"

  gameData <- case dataResult of
    Left err -> do
      putStrLn $ "Error loading game data: " ++ err
      exitFailure
    Right gd -> return gd

  -- Execute based on mode
  case cliMode options of
    ParseMode tslString -> do
      -- Parse mode: decode and display TSL
      runParseMode gameData tslString

    REPLMode maybeInitialState -> do
      -- REPL mode: interactive session with logging
      runREPLMode gameData maybeInitialState

-- | Run parse mode (decode and display TSL)
runParseMode :: GameData -> T.Text -> IO ()
runParseMode gameData tslString = do
  case parseGameState tslString of
    Left err -> do
      putStrLn "Parse error:"
      putStrLn $ show err
      exitFailure
    Right gameState -> displayParseMode gameData gameState

-- | Display game state in parse mode
displayParseMode :: GameData -> GameState -> IO ()
displayParseMode gd gs = do
  putStrLn "=== TFT Game State ==="
  putStrLn ""

  putStrLn $ "Player Level: " ++ show (gsLevel gs)
  putStrLn $ "Stage-Round:  " ++ show (gsStage gs) ++ "-" ++ show (gsRound gs)
  putStrLn $ "Gold:         " ++ show (gsGold gs)
  putStrLn $ "Health:       " ++ show (gsHealth gs)
  putStrLn ""

  -- Display board
  unless (null $ gsBoard gs) $ do
    putStrLn "Board:"
    mapM_ (displayChampion gd) (gsBoard gs)
    putStrLn ""

  -- Display items
  unless (null $ gsItems gs) $ do
    putStrLn "Items:"
    mapM_ (displayItem gd) (gsItems gs)
    putStrLn ""

  -- Display augments
  unless (null $ gsAugments gs) $ do
    putStrLn "Augments:"
    mapM_ (displayAugment gd) (gsAugments gs)

displayChampion :: GameData -> Champion -> IO ()
displayChampion gd champ = do
  let shorthand = champShorthand champ
  let stars = champStars champ
  case lookupChampion gd shorthand of
    Nothing -> putStrLn $ "  [" ++ show stars ++ "★] " ++ show shorthand ++ " (unknown champion)"
    Just champData -> do
      let name = T.unpack $ cdName champData
      let costStr = case cdCost champData of
            Just c -> "Cost: " ++ show c
            Nothing -> "Summoned Unit"
      let traits = T.unpack $ T.intercalate ", " (cdTraits champData)
      putStrLn $ "  [" ++ show stars ++ "★] " ++ name ++ " (" ++ costStr ++ ")"
      unless (null traits) $ putStrLn $ "      Traits: " ++ traits

displayItem :: GameData -> ItemShorthand -> IO ()
displayItem gd shorthand = do
  case lookupItem gd shorthand of
    Nothing -> putStrLn $ "  " ++ show shorthand ++ " (unknown item)"
    Just itemData -> do
      let name = T.unpack $ idName itemData
      let itemType = T.unpack $ idType itemData
      putStrLn $ "  " ++ name ++ " [" ++ itemType ++ "]"

displayAugment :: GameData -> AugmentShorthand -> IO ()
displayAugment gd shorthand = do
  case lookupAugment gd shorthand of
    Nothing -> putStrLn $ "  " ++ show shorthand ++ " (unknown augment)"
    Just augData -> do
      let name = T.unpack $ adName augData
      let tags = T.unpack $ adTags augData
      putStrLn $ "  " ++ name ++ " [" ++ tags ++ "]"

-- | Run REPL mode with session logging
runREPLMode :: GameData -> Maybe T.Text -> IO ()
runREPLMode gameData maybeInitialState = do
  -- Generate timestamped log filename
  now <- getZonedTime
  let timestamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now
  let logFilename = "tft-session-" ++ timestamp ++ ".log"

  -- Open log file
  logHandle <- openFile logFilename WriteMode

  -- Log the create command with initial state
  case maybeInitialState of
    Nothing -> hPutStrLn logHandle "create"
    Just tsl -> hPutStrLn logHandle $ "create " ++ T.unpack tsl

  -- Run REPL
  runREPL maybeInitialState gameData logHandle

  -- Write end marker and close log
  hPutStrLn logHandle "end"
  hClose logHandle

  putStrLn $ "\nSession log saved to: " ++ logFilename

-- Helper function
unless :: Bool -> IO () -> IO ()
unless p action = if p then return () else action
