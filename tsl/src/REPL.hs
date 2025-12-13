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
import Control.Monad (when, foldM)
import Data.IORef
import Data.List (find, isPrefixOf)
import Data.Char (isDigit, toLower)

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
  | Add [Text] Bool   -- Add entities (CIA), Bool = print after
  | Upgrade Text    -- Upgrade champion or item
  | Sell [Text]       -- Sell champions
  | Find Text       -- Find entities by fuzzy search
  | Level (Maybe Int) -- Set or increment level
  | Round (Maybe (Int, Int)) -- Set stage-round
  | Money Int       -- Set gold amount
  | Unknown Text    -- Unknown command
  deriving (Show, Eq)

-- | List of all available commands with descriptions
allCommands :: [(Text, Text)]
allCommands =
  [ ("print", "Display the current game state as TSL")
  , ("add <entity>... [*]", "Add one or more champions/items/augments (optional * to print after)")
  , ("upgrade <entity>", "Upgrade a champion's stars or combine items")
  , ("sell <champion>...", "Sell one or more champions and gain gold")
  , ("find <query>", "Search for champions/items/augments by name or shorthand")
  , ("level [n]", "Set level to n, or increment by 1 if no argument")
  , ("round [s r]", "Set stage-round to s-r, or increment if no arguments")
  , ("money <n>", "Set gold to n")
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
            entityParts = if hasAsterisk then init (tail parts) else tail parts
            entities = map T.strip entityParts
        in Add entities hasAsterisk

    "a" | length parts >= 2 ->
        let hasAsterisk = T.isSuffixOf "*" (last parts)
            entityParts = if hasAsterisk then init (tail parts) else tail parts
            entities = map T.strip entityParts
        in Add entities hasAsterisk

    "upgrade" | length parts >= 2 -> Upgrade (T.unwords $ tail parts)
    "u" | length parts >= 2 -> Upgrade (T.unwords $ tail parts)

    "sell" | length parts >= 2 -> Sell (map T.strip $ tail parts)
    "s" | length parts >= 2 -> Sell (map T.strip $ tail parts)

    "find" | length parts >= 2 -> Find (T.unwords $ tail parts)
    "f" | length parts >= 2 -> Find (T.unwords $ tail parts)

    "level" -> parseLevel (tail parts)
    "l" -> parseLevel (tail parts)

    "round" -> parseRound (tail parts)
    "r" -> parseRound (tail parts)

    "money" | length parts >= 2 -> parseMoney (tail parts)
    "m" | length parts >= 2 -> parseMoney (tail parts)

    "" -> Unknown ""
    _ -> Unknown trimmed
  where
    parseLevel [] = Level Nothing
    parseLevel (n:_) = case reads (T.unpack n) of
      [(val, "")] -> Level (Just val)
      _ -> Unknown input

    parseRound [] = Round Nothing
    parseRound [s, rd] = case (reads (T.unpack s), reads (T.unpack rd)) of
      ([(stage, "")], [(rnd, "")]) -> Round (Just (stage, rnd))
      _ -> Unknown input
    parseRound _ = Unknown input

    parseMoney (n:_) = case reads (T.unpack n) of
      [(val, "")] -> Money val
      _ -> Unknown input
    parseMoney _ = Unknown input

-- | Completion function for Haskeline
completeFunc :: GameData -> CompletionFunc IO
completeFunc gameData = completeWord Nothing " \t" $ \prefix -> do
  let prefixLower = map toLower prefix

      -- Command completions (no shorthands needed)
      commandCompletions = ["print", "add", "upgrade", "sell", "find", "level", "round", "money", "help", "quit", "exit", "p", "a", "u", "s", "f", "l", "r", "m", "h", "q"]

      -- Build entity completions with format: "name (SHORTHAND)"
      -- This creates completions that match on both name and shorthand
      championCompletions =
        [ (T.unpack $ T.toLower $ cdName champ, T.unpack sh)
        | champ <- M.elems (gdChampions gameData)
        , let ChampionShorthand sh = cdShorthand champ
        ]

      itemCompletions =
        [ (T.unpack $ T.toLower $ idName item, T.unpack sh)
        | item <- M.elems (gdItems gameData)
        , let ItemShorthand sh = idShorthand item
        ]

      augmentCompletions =
        [ (T.unpack $ T.toLower $ adName aug, T.unpack sh)
        | aug <- M.elems (gdAugments gameData)
        , let AugmentShorthand sh = adShorthand aug
        ]

      -- Filter commands
      matchingCommands = filter (prefixLower `isPrefixOf`) commandCompletions

      -- Filter entities - match on either name or shorthand
      matchingChampions =
        [ makeCompletion name sh
        | (name, sh) <- championCompletions
        , prefixLower `isPrefixOf` name || prefixLower `isPrefixOf` map toLower sh
        ]

      matchingItems =
        [ makeCompletion name sh
        | (name, sh) <- itemCompletions
        , prefixLower `isPrefixOf` name || prefixLower `isPrefixOf` map toLower sh
        ]

      matchingAugments =
        [ makeCompletion name sh
        | (name, sh) <- augmentCompletions
        , prefixLower `isPrefixOf` name || prefixLower `isPrefixOf` map toLower sh
        ]

      -- Combine all matches
      allMatches = map simpleCompletion matchingCommands
                ++ matchingChampions
                ++ matchingItems
                ++ matchingAugments

  return allMatches

-- | Create a completion with name and shorthand
-- The replacement is just the name, but the display shows "name (SHORTHAND)"
makeCompletion :: String -> String -> Completion
makeCompletion name shorthand = Completion
  { replacement = name
  , display = name ++ " (" ++ shorthand ++ ")"
  , isFinished = True
  }

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

  Add entities printAfter -> do
    handleAdd state entities printAfter
    return True

  Upgrade entity -> do
    handleUpgrade state entity
    return True

  Sell champions -> do
    handleSell state champions
    return True

  Find query -> do
    handleFind state query
    return True

  Level maybeLevel -> do
    handleLevel state maybeLevel
    return True

  Round maybeRound -> do
    handleRound state maybeRound
    return True

  Money amount -> do
    handleMoney state amount
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
handleAdd :: REPLState -> [Text] -> Bool -> IO ()
handleAdd state entities printAfter = do
  currentState <- readIORef (replGameStateRef state)

  -- Get or create game state
  gs <- case currentState of
    Nothing -> do
      -- Create default game state
      let defaultState = GameState
            { gsLevel = 1
            , gsStage = 1
            , gsRound = 1
            , gsGold = 0
            , gsHealth = 100
            , gsBoard = []
            , gsItems = []
            , gsAugments = []
            }
      writeIORef (replGameStateRef state) (Just defaultState)
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn "Created new game state (Level 1, Stage 1-1, 0g, 100h)"
      setSGR [Reset]
      return defaultState
    Just gs -> return gs

  -- Process each entity in sequence
  finalState <- foldM (processEntity state) gs entities

  -- Update the state after processing all entities
  writeIORef (replGameStateRef state) (Just finalState)

  when printAfter $ do
    putStrLn ""
    printGameState state
  where
    processEntity :: REPLState -> GameState -> Text -> IO GameState
    processEntity st gs entity = do
      let (stars, shorthand) = parseEntityInput entity

      result <- addEntity (replGameData st) gs stars shorthand

      case result of
        Left err -> do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn $ "Error: " ++ err
          setSGR [Reset]
          return gs  -- Return unchanged state on error
        Right newState -> do
          setSGR [SetColor Foreground Vivid Green]
          putStrLn $ "Added " ++ T.unpack shorthand
          setSGR [Reset]
          return newState  -- Return updated state

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

-- | Handle sell command (with multiple champions)
handleSell :: REPLState -> [Text] -> IO ()
handleSell state champions = do
  currentState <- readIORef (replGameStateRef state)

  case currentState of
    Nothing -> do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn "Error: No game state loaded."
      setSGR [Reset]
    Just gs -> do
      -- Process each champion in sequence
      finalState <- foldM processChampion gs champions
      writeIORef (replGameStateRef state) (Just finalState)
  where
    processChampion :: GameState -> Text -> IO GameState
    processChampion gs champion = do
      let shorthand = T.toUpper $ T.strip champion
          champSh = ChampionShorthand shorthand

      case find (\c -> champShorthand c == champSh) (gsBoard gs) of
        Nothing -> do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn $ "Error: Champion not found: " ++ T.unpack shorthand
          setSGR [Reset]
          return gs  -- Return unchanged state on error
        Just _ -> do
          -- Calculate gold from sell
          case lookupChampion (replGameData state) champSh of
            Nothing -> do
              setSGR [SetColor Foreground Vivid Red]
              putStrLn "Error: Champion data not found"
              setSGR [Reset]
              return gs  -- Return unchanged state on error
            Just champData -> do
              let cost = maybe 1 id (cdCost champData)
                  level = gsLevel gs
                  goldGained = level * cost - (if level == 1 then 0 else 1)
                  newBoard = filter (\c -> champShorthand c /= champSh) (gsBoard gs)
                  newState = gs
                    { gsBoard = newBoard
                    , gsGold = gsGold gs + goldGained
                    }

              setSGR [SetColor Foreground Vivid Green]
              putStrLn $ "Sold " ++ T.unpack shorthand ++ " for " ++ show goldGained ++ " gold"
              setSGR [Reset]
              return newState  -- Return updated state

-- | Handle level command
handleLevel :: REPLState -> Maybe Int -> IO ()
handleLevel state maybeLevel = do
  currentState <- readIORef (replGameStateRef state)

  -- Get or create game state
  gs <- case currentState of
    Nothing -> do
      let defaultState = GameState
            { gsLevel = 1
            , gsStage = 1
            , gsRound = 1
            , gsGold = 0
            , gsHealth = 100
            , gsBoard = []
            , gsItems = []
            , gsAugments = []
            }
      writeIORef (replGameStateRef state) (Just defaultState)
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn "Created new game state (Level 1, Stage 1-1, 0g, 100h)"
      setSGR [Reset]
      return defaultState
    Just gs -> return gs

  let newLevel = case maybeLevel of
        Nothing -> min 10 (gsLevel gs + 1)  -- Increment, max 10
        Just n -> max 1 (min 10 n)  -- Set to n, clamped to 1-10

  let newState = gs { gsLevel = newLevel }
  writeIORef (replGameStateRef state) (Just newState)

  setSGR [SetColor Foreground Vivid Green]
  putStrLn $ "Level set to " ++ show newLevel
  setSGR [Reset]

-- | Handle round command
handleRound :: REPLState -> Maybe (Int, Int) -> IO ()
handleRound state maybeRound = do
  currentState <- readIORef (replGameStateRef state)

  -- Get or create game state
  gs <- case currentState of
    Nothing -> do
      let defaultState = GameState
            { gsLevel = 1
            , gsStage = 1
            , gsRound = 1
            , gsGold = 0
            , gsHealth = 100
            , gsBoard = []
            , gsItems = []
            , gsAugments = []
            }
      writeIORef (replGameStateRef state) (Just defaultState)
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn "Created new game state (Level 1, Stage 1-1, 0g, 100h)"
      setSGR [Reset]
      return defaultState
    Just gs -> return gs

  let (newStage, newRound) = case maybeRound of
        Nothing ->
          -- Increment round
          if gsRound gs >= 7
          then (gsStage gs + 1, 1)  -- Next stage
          else (gsStage gs, gsRound gs + 1)  -- Next round
        Just (s, r) -> (max 1 s, max 1 (min 7 r))  -- Set to s-r, round clamped to 1-7

  let newState = gs { gsStage = newStage, gsRound = newRound }
  writeIORef (replGameStateRef state) (Just newState)

  setSGR [SetColor Foreground Vivid Green]
  putStrLn $ "Round set to " ++ show newStage ++ "-" ++ show newRound
  setSGR [Reset]

-- | Handle money command
handleMoney :: REPLState -> Int -> IO ()
handleMoney state amount = do
  currentState <- readIORef (replGameStateRef state)

  -- Get or create game state
  gs <- case currentState of
    Nothing -> do
      let defaultState = GameState
            { gsLevel = 1
            , gsStage = 1
            , gsRound = 1
            , gsGold = 0
            , gsHealth = 100
            , gsBoard = []
            , gsItems = []
            , gsAugments = []
            }
      writeIORef (replGameStateRef state) (Just defaultState)
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn "Created new game state (Level 1, Stage 1-1, 0g, 100h)"
      setSGR [Reset]
      return defaultState
    Just gs -> return gs

  let newGold = max 0 amount  -- Can't have negative gold

  let newState = gs { gsGold = newGold }
  writeIORef (replGameStateRef state) (Just newState)

  setSGR [SetColor Foreground Vivid Green]
  putStrLn $ "Gold set to " ++ show newGold
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
