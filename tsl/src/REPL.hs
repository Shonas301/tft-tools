{-# LANGUAGE OverloadedStrings #-}

module REPL
  ( REPLState(..)
  , REPLCommand(..)
  , runREPL
  , parseCommand
  , executeCommand
  , allCommands
  ) where

import Types
import Parser (parseGameState, TFTParseError)
import DataLoader
import qualified Data.Text as T
import Data.Text (Text)
import System.Console.ANSI
import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe)
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.IO (Handle, hPutStrLn)

-- | REPL state containing current game state and session log handle
data REPLState = REPLState
  { replGameState :: Maybe GameState
  , replGameData :: GameData
  , replLogHandle :: Handle
  } deriving (Show)

-- | REPL commands
data REPLCommand
  = Print           -- Display current TSL
  | Help            -- Show available commands
  | Quit            -- Exit REPL
  | Unknown Text    -- Unknown command
  deriving (Show, Eq)

-- | List of all available commands with descriptions
allCommands :: [(Text, Text)]
allCommands =
  [ ("print", "Display the current game state as TSL")
  , ("help", "Show this help message")
  , ("quit", "Exit the REPL (aliases: exit, q)")
  , ("exit", "Exit the REPL")
  ]

-- | Parse a command from user input
parseCommand :: Text -> REPLCommand
parseCommand input =
  case T.strip $ T.toLower input of
    "print" -> Print
    "p" -> Print
    "help" -> Help
    "h" -> Help
    "?" -> Help
    "quit" -> Quit
    "exit" -> Quit
    "q" -> Quit
    "" -> Unknown ""
    cmd -> Unknown cmd

-- | Run the REPL loop
runREPL :: Maybe Text -> GameData -> Handle -> IO ()
runREPL initialState gameData logHandle = do
  -- Parse initial state if provided
  initialGameState <- case initialState of
    Nothing -> return Nothing
    Just tsl -> case parseGameState tsl of
      Left err -> do
        putStrLn $ "Error parsing initial state: " ++ show err
        return Nothing
      Right gs -> return (Just gs)

  -- Print welcome message
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn "╔═══════════════════════════════════════════════════════════╗"
  putStrLn "║          TFT-DSL Interactive REPL                       ║"
  putStrLn "║  Type 'help' for available commands, 'quit' to exit     ║"
  putStrLn "╚═══════════════════════════════════════════════════════════╝"
  setSGR [Reset]
  putStrLn ""

  -- Initialize REPL state
  let state = REPLState
        { replGameState = initialGameState
        , replGameData = gameData
        , replLogHandle = logHandle
        }

  -- Start REPL loop
  replLoop state

-- | Main REPL loop
replLoop :: REPLState -> IO ()
replLoop state = do
  -- Display prompt
  setSGR [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity]
  putStr "tft> "
  setSGR [Reset]
  hFlush stdout

  -- Read user input
  input <- T.pack <$> getLine

  -- Log the command
  hPutStrLn (replLogHandle state) (T.unpack input)

  -- Parse and execute command
  let cmd = parseCommand input
  (continue, newState) <- executeCommand cmd state

  -- Continue or exit
  if continue
    then replLoop newState
    else return ()

-- | Execute a REPL command
executeCommand :: REPLCommand -> REPLState -> IO (Bool, REPLState)
executeCommand cmd state = case cmd of
  Print -> do
    printGameState state
    return (True, state)

  Help -> do
    showHelp
    return (True, state)

  Quit -> do
    setSGR [SetColor Foreground Vivid Yellow]
    putStrLn "Goodbye!"
    setSGR [Reset]
    return (False, state)

  Unknown "" -> return (True, state)  -- Empty input, just show prompt again

  Unknown cmdText -> do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn $ "Unknown command: " ++ T.unpack cmdText
    setSGR [Reset]
    putStrLn "Type 'help' to see available commands."
    putStrLn ""
    return (True, state)

-- | Print the current game state
printGameState :: REPLState -> IO ()
printGameState state = do
  case replGameState state of
    Nothing -> do
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn "No game state loaded. The state is empty."
      setSGR [Reset]
    Just gs -> do
      setSGR [SetColor Foreground Vivid Cyan]
      putStrLn "Current game state:"
      setSGR [Reset]
      displayGameState (replGameData state) gs
  putStrLn ""

-- | Display game state (same as Main.hs)
displayGameState :: GameData -> GameState -> IO ()
displayGameState gd gs = do
  putStrLn $ "Player Level: " ++ show (gsLevel gs)
  putStrLn $ "Stage-Round:  " ++ show (gsStage gs) ++ "-" ++ show (gsRound gs)
  putStrLn $ "Gold:         " ++ show (gsGold gs)
  putStrLn $ "Health:       " ++ show (gsHealth gs)

  unless (null $ gsBoard gs) $ do
    putStrLn ""
    putStrLn "Board:"
    mapM_ (displayChampion gd) (gsBoard gs)

  unless (null $ gsItems gs) $ do
    putStrLn ""
    putStrLn "Items:"
    mapM_ (displayItem gd) (gsItems gs)

  unless (null $ gsAugments gs) $ do
    putStrLn ""
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

-- | Show help message with available commands
showHelp :: IO ()
showHelp = do
  setSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]
  putStrLn "Available Commands:"
  setSGR [Reset]
  putStrLn ""
  mapM_ printCommand allCommands
  putStrLn ""
  where
    printCommand (cmd, desc) = do
      setSGR [SetColor Foreground Vivid Yellow]
      putStr $ "  " ++ T.unpack cmd
      setSGR [Reset]
      putStrLn $ " - " ++ T.unpack desc

-- Helper function
unless :: Bool -> IO () -> IO ()
unless p action = if p then return () else action
