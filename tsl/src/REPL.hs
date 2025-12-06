{-# LANGUAGE OverloadedStrings #-}

module REPL
  ( REPLState(..)
  , REPLCommand(..)
  , runREPL
  , parseCommand
  , executeCommand
  , allCommands
  , parseEntityInput
  ) where

import Types
import Parser (parseGameState)
import DataLoader
import Encoder
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import System.Console.ANSI
import System.IO (hFlush, stdout, Handle, hPutStrLn)
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (find, isPrefixOf)
import Data.Char (isDigit)

-- | REPL state with mutable game state
data REPLState = REPLState
  { replGameStateRef :: IORef (Maybe GameState)
  , replGameData :: GameData
  , replLogHandle :: Handle
  }

-- | REPL commands
data REPLCommand
  = Print           -- Display current TSL
  | Help            -- Show available commands
  | Quit            -- Exit REPL
  | Add Text Bool   -- Add entity (CIA), Bool = print after
  | Upgrade Text    -- Upgrade champion or item
  | Sell Text       -- Sell champion
  | Find Text       -- Find entities by fuzzy search
  | Unknown Text    -- Unknown command
  deriving (Show, Eq)

-- | List of all available commands with descriptions
allCommands :: [(Text, Text)]
allCommands =
  [ ("print", "Display the current game state as TSL")
  , ("add <entity> [*]", "Add champion/item/augment (optional * to print after)")
  , ("upgrade <entity>", "Upgrade a champion's stars or combine items")
  , ("sell <champion>", "Sell a champion and gain gold")
  , ("find <query>", "Search for champions/items/augments by name or shorthand")
  , ("help", "Show this help message")
  , ("quit", "Exit the REPL (aliases: exit, q)")
  ]

-- | Parse a command from user input
parseCommand :: Text -> REPLCommand
parseCommand input =
  let trimmed = T.strip input
      parts = T.words trimmed
      lowerFirst = if null parts then "" else T.toLower (head parts)
  in case lowerFirst of
    "print" | length parts == 1 -> Print
    "p" | length parts == 1 -> Print

    "help" | length parts == 1 -> Help
    "h" | length parts == 1 -> Help
    "?" | length parts == 1 -> Help

    "quit" | length parts == 1 -> Quit
    "exit" | length parts == 1 -> Quit
    "q" | length parts == 1 -> Quit

    "add" | length parts >= 2 ->
        let hasAsterisk = T.isSuffixOf "*" (last parts)
            entity = if hasAsterisk
                    then T.unwords (tail $ init parts) <> last parts
                    else T.unwords (tail parts)
        in Add (T.strip $ T.replace "*" "" entity) hasAsterisk

    "a" | length parts >= 2 ->
        let hasAsterisk = T.isSuffixOf "*" (last parts)
            entity = if hasAsterisk
                    then T.unwords (tail $ init parts) <> last parts
                    else T.unwords (tail parts)
        in Add (T.strip $ T.replace "*" "" entity) hasAsterisk

    "upgrade" | length parts >= 2 -> Upgrade (T.unwords $ tail parts)
    "u" | length parts >= 2 -> Upgrade (T.unwords $ tail parts)

    "sell" | length parts >= 2 -> Sell (T.unwords $ tail parts)
    "s" | length parts >= 2 -> Sell (T.unwords $ tail parts)

    "find" | length parts >= 2 -> Find (T.unwords $ tail parts)
    "f" | length parts >= 2 -> Find (T.unwords $ tail parts)

    "" -> Unknown ""
    _ -> Unknown trimmed

-- | Completion function for Haskeline
completeFunc :: GameData -> CompletionFunc IO
completeFunc gameData = completeWord Nothing " \t" $ \prefix -> do
  let prefixText = T.pack prefix
      prefixLower = T.toLower prefixText

      -- Get all possible completions
      commandCompletions = ["print", "add", "upgrade", "sell", "find", "help", "quit", "exit", "p", "a", "u", "s", "f", "h", "q"]

      -- Get entity completions from game data
      championNames = map (T.unpack . T.toLower . cdName) (M.elems $ gdChampions gameData)
      championShorthands = map (\(ChampionShorthand sh) -> T.unpack $ T.toLower sh) (M.keys $ gdChampions gameData)

      itemNames = map (T.unpack . T.toLower . idName) (M.elems $ gdItems gameData)
      itemShorthands = map (\(ItemShorthand sh) -> T.unpack $ T.toLower sh) (M.keys $ gdItems gameData)

      augmentNames = map (T.unpack . T.toLower . adName) (M.elems $ gdAugments gameData)
      augmentShorthands = map (\(AugmentShorthand sh) -> T.unpack $ T.toLower sh) (M.keys $ gdAugments gameData)

      allCompletions = commandCompletions
                    ++ championNames ++ championShorthands
                    ++ itemNames ++ itemShorthands
                    ++ augmentNames ++ augmentShorthands

      -- Filter completions by prefix
      matches = filter (prefix `isPrefixOf`) allCompletions

  return $ map simpleCompletion matches

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

  -- Create mutable state reference
  stateRef <- newIORef initialGameState

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
        { replGameStateRef = stateRef
        , replGameData = gameData
        , replLogHandle = logHandle
        }

  -- Start REPL loop with Haskeline
  let settings = Settings
        { complete = completeFunc gameData
        , historyFile = Nothing
        , autoAddHistory = True
        }

  runInputT settings $ replLoop state

-- | Main REPL loop with Haskeline
replLoop :: REPLState -> InputT IO ()
replLoop state = do
  -- Read user input with tab completion
  minput <- getInputLine "\ESC[92;1mtft> \ESC[0m"

  case minput of
    Nothing -> return ()  -- EOF (Ctrl-D)
    Just input -> do
      let inputText = T.pack input

      -- Log the command
      liftIO $ hPutStrLn (replLogHandle state) input

      -- Parse and execute command
      let cmd = parseCommand inputText
      continue <- liftIO $ executeCommand cmd state

      -- Continue or exit
      if continue
        then replLoop state
        else return ()

-- | Execute a REPL command
executeCommand :: REPLCommand -> REPLState -> IO Bool
executeCommand cmd state = case cmd of
  Print -> do
    printGameState state
    return True

  Help -> do
    showHelp
    return True

  Quit -> do
    setSGR [SetColor Foreground Vivid Yellow]
    putStrLn "Goodbye!"
    setSGR [Reset]
    return False

  Add entity printAfter -> do
    handleAdd state entity printAfter
    return True

  Upgrade entity -> do
    handleUpgrade state entity
    return True

  Sell champion -> do
    handleSell state champion
    return True

  Find query -> do
    handleFind state query
    return True

  Unknown "" -> return True  -- Empty input

  Unknown cmdText -> do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn $ "Unknown command: " ++ T.unpack cmdText
    setSGR [Reset]
    putStrLn "Type 'help' to see available commands."
    putStrLn ""
    return True

-- | Handle add command
handleAdd :: REPLState -> Text -> Bool -> IO ()
handleAdd state entity printAfter = do
  currentState <- readIORef (replGameStateRef state)

  case currentState of
    Nothing -> do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn "Error: No game state loaded. Cannot add to empty state."
      setSGR [Reset]
    Just gs -> do
      -- Parse entity (could be: ANI, 2ANI, IE, HG, etc.)
      let (stars, shorthand) = parseEntityInput entity

      -- Try to determine type and add
      result <- addEntity (replGameData state) gs stars shorthand

      case result of
        Left err -> do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn $ "Error: " ++ err
          setSGR [Reset]
        Right newState -> do
          writeIORef (replGameStateRef state) (Just newState)
          setSGR [SetColor Foreground Vivid Green]
          putStrLn $ "Added " ++ T.unpack shorthand
          setSGR [Reset]

          when printAfter $ do
            putStrLn ""
            printGameState state

-- | Parse entity input to extract stars and shorthand
parseEntityInput :: Text -> (Int, Text)
parseEntityInput input =
  let stripped = T.strip input
      upperInput = T.toUpper stripped
  in if T.null upperInput
     then (1, "")
     else if isDigit (T.head upperInput)
          then (read [T.head upperInput], T.tail upperInput)
          else (1, upperInput)

-- | Add an entity to the game state
addEntity :: GameData -> GameState -> Int -> Text -> IO (Either String GameState)
addEntity gameData gs stars shorthand = do
  let champSh = ChampionShorthand shorthand
      itemSh = ItemShorthand shorthand
      augSh = AugmentShorthand shorthand

  -- Try champion first
  case lookupChampion gameData champSh of
    Just _ -> return $ Right $ gs
      { gsBoard = gsBoard gs ++ [Champion stars champSh]
      }
    Nothing ->
      -- Try item
      case lookupItem gameData itemSh of
        Just _ -> return $ Right $ gs
          { gsItems = gsItems gs ++ [itemSh]
          }
        Nothing ->
          -- Try augment
          case lookupAugment gameData augSh of
            Just _ ->
              if length (gsAugments gs) >= 3
              then return $ Left "Cannot add more than 3 augments"
              else return $ Right $ gs
                { gsAugments = gsAugments gs ++ [augSh]
                }
            Nothing -> return $ Left $ "Unknown entity: " ++ T.unpack shorthand

-- | Handle upgrade command
handleUpgrade :: REPLState -> Text -> IO ()
handleUpgrade state entity = do
  currentState <- readIORef (replGameStateRef state)

  case currentState of
    Nothing -> do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn "Error: No game state loaded."
      setSGR [Reset]
    Just gs -> do
      let shorthand = T.toUpper $ T.strip entity
          champSh = ChampionShorthand shorthand
          itemSh = ItemShorthand shorthand

      -- Try champion upgrade
      case find (\c -> champShorthand c == champSh) (gsBoard gs) of
        Just champ ->
          if champStars champ >= 3
          then do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "Error: Champion is already 3-star"
            setSGR [Reset]
          else do
            let newBoard = map (\c -> if champShorthand c == champSh
                                      then c { champStars = champStars c + 1 }
                                      else c) (gsBoard gs)
                newState = gs { gsBoard = newBoard }
            writeIORef (replGameStateRef state) (Just newState)
            setSGR [SetColor Foreground Vivid Green]
            putStrLn $ "Upgraded " ++ T.unpack shorthand ++ " to " ++ show (champStars champ + 1) ++ "-star"
            setSGR [Reset]

        Nothing ->
          -- Try item upgrade (for now, just show message - full implementation would need component recipes)
          case lookupItem (replGameData state) itemSh of
            Just _ -> do
              setSGR [SetColor Foreground Vivid Yellow]
              putStrLn "Item upgrade: Component combining not yet fully implemented"
              setSGR [Reset]
            Nothing -> do
              setSGR [SetColor Foreground Vivid Red]
              putStrLn $ "Error: Entity not found in game state: " ++ T.unpack shorthand
              setSGR [Reset]

-- | Handle sell command
handleSell :: REPLState -> Text -> IO ()
handleSell state champion = do
  currentState <- readIORef (replGameStateRef state)

  case currentState of
    Nothing -> do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn "Error: No game state loaded."
      setSGR [Reset]
    Just gs -> do
      let shorthand = T.toUpper $ T.strip champion
          champSh = ChampionShorthand shorthand

      case find (\c -> champShorthand c == champSh) (gsBoard gs) of
        Nothing -> do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn $ "Error: Champion not found: " ++ T.unpack shorthand
          setSGR [Reset]
        Just _ -> do
          -- Calculate gold from sell
          case lookupChampion (replGameData state) champSh of
            Nothing -> do
              setSGR [SetColor Foreground Vivid Red]
              putStrLn "Error: Champion data not found"
              setSGR [Reset]
            Just champData -> do
              let cost = maybe 1 id (cdCost champData)
                  level = gsLevel gs
                  goldGained = level * cost - (if level == 1 then 0 else 1)
                  newBoard = filter (\c -> champShorthand c /= champSh) (gsBoard gs)
                  newState = gs
                    { gsBoard = newBoard
                    , gsGold = gsGold gs + goldGained
                    }

              writeIORef (replGameStateRef state) (Just newState)
              setSGR [SetColor Foreground Vivid Green]
              putStrLn $ "Sold " ++ T.unpack shorthand ++ " for " ++ show goldGained ++ " gold"
              putStrLn $ "New gold: " ++ show (gsGold gs + goldGained)
              setSGR [Reset]

-- | Handle find command
handleFind :: REPLState -> Text -> IO ()
handleFind state query = do
  let gameData = replGameData state
      queryLower = T.toLower query

      -- Search champions (convert Map to list)
      champMatches = filter (matchesQuery queryLower) (M.elems $ gdChampions gameData)

      -- Search items (convert Map to list)
      itemMatches = filter (matchesItemQuery queryLower) (M.elems $ gdItems gameData)

      -- Search augments (convert Map to list)
      augMatches = filter (matchesAugmentQuery queryLower) (M.elems $ gdAugments gameData)

      totalMatches = length champMatches + length itemMatches + length augMatches

  if totalMatches == 0
    then do
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn $ "No matches found for: " ++ T.unpack query
      setSGR [Reset]
    else do
      setSGR [SetColor Foreground Vivid Cyan]
      putStrLn $ "Found " ++ show totalMatches ++ " match(es) for: " ++ T.unpack query
      setSGR [Reset]
      putStrLn ""

      unless (null champMatches) $ do
        setSGR [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity]
        putStrLn "Champions:"
        setSGR [Reset]
        mapM_ displayChampionMatch (take 10 champMatches)
        when (length champMatches > 10) $
          putStrLn $ "  ... and " ++ show (length champMatches - 10) ++ " more"
        putStrLn ""

      unless (null itemMatches) $ do
        setSGR [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]
        putStrLn "Items:"
        setSGR [Reset]
        mapM_ displayItemMatch (take 10 itemMatches)
        when (length itemMatches > 10) $
          putStrLn $ "  ... and " ++ show (length itemMatches - 10) ++ " more"
        putStrLn ""

      unless (null augMatches) $ do
        setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity]
        putStrLn "Augments:"
        setSGR [Reset]
        mapM_ displayAugmentMatch (take 10 augMatches)
        when (length augMatches > 10) $
          putStrLn $ "  ... and " ++ show (length augMatches - 10) ++ " more"

-- | Check if a champion matches the query
matchesQuery :: Text -> ChampionData -> Bool
matchesQuery query champ =
  let nameLower = T.toLower (cdName champ)
      ChampionShorthand sh = cdShorthand champ
      shLower = T.toLower sh
  in query `T.isInfixOf` nameLower || query `T.isInfixOf` shLower

-- | Check if an item matches the query
matchesItemQuery :: Text -> ItemData -> Bool
matchesItemQuery query item =
  let nameLower = T.toLower (idName item)
      ItemShorthand sh = idShorthand item
      shLower = T.toLower sh
  in query `T.isInfixOf` nameLower || query `T.isInfixOf` shLower

-- | Check if an augment matches the query
matchesAugmentQuery :: Text -> AugmentData -> Bool
matchesAugmentQuery query aug =
  let nameLower = T.toLower (adName aug)
      AugmentShorthand sh = adShorthand aug
      shLower = T.toLower sh
  in query `T.isInfixOf` nameLower || query `T.isInfixOf` shLower

-- | Display a champion match result
displayChampionMatch :: ChampionData -> IO ()
displayChampionMatch champ = do
  let ChampionShorthand sh = cdShorthand champ
  setSGR [SetColor Foreground Vivid Yellow]
  putStr $ "  [" ++ T.unpack sh ++ "]"
  setSGR [Reset]
  putStr " "
  putStr $ T.unpack (cdName champ)
  case cdCost champ of
    Just cost -> putStr $ " (Cost: " ++ show cost ++ ")"
    Nothing -> putStr " (Summoned Unit)"
  unless (null $ cdTraits champ) $
    putStr $ " - " ++ T.unpack (T.intercalate ", " (cdTraits champ))
  putStrLn ""

-- | Display an item match result
displayItemMatch :: ItemData -> IO ()
displayItemMatch item = do
  let ItemShorthand sh = idShorthand item
  setSGR [SetColor Foreground Vivid Yellow]
  putStr $ "  [" ++ T.unpack sh ++ "]"
  setSGR [Reset]
  putStr " "
  putStr $ T.unpack (idName item)
  putStr $ " [" ++ T.unpack (idType item) ++ "]"
  putStrLn ""

-- | Display an augment match result
displayAugmentMatch :: AugmentData -> IO ()
displayAugmentMatch aug = do
  let AugmentShorthand sh = adShorthand aug
  setSGR [SetColor Foreground Vivid Yellow]
  putStr $ "  [" ++ T.unpack sh ++ "]"
  setSGR [Reset]
  putStr " "
  putStr $ T.unpack (adName aug)
  putStr $ " [" ++ T.unpack (adTags aug) ++ "]"
  putStrLn ""

-- | Print the current game state
printGameState :: REPLState -> IO ()
printGameState state = do
  currentState <- readIORef (replGameStateRef state)
  case currentState of
    Nothing -> do
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn "No game state loaded. The state is empty."
      setSGR [Reset]
    Just gs -> do
      setSGR [SetColor Foreground Vivid Cyan]
      putStrLn "Current game state (TSL):"
      setSGR [Reset]
      putStrLn $ T.unpack $ encodeGameState gs
      putStrLn ""
      setSGR [SetColor Foreground Vivid Cyan]
      putStrLn "Detailed view:"
      setSGR [Reset]
      displayGameState (replGameData state) gs
  putStrLn ""

-- | Display game state (detailed view)
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

-- Helper functions
unless :: Bool -> IO () -> IO ()
unless p action = if p then return () else action

when :: Bool -> IO () -> IO ()
when p action = if p then action else return ()
