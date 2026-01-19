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
import Probability
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import System.Console.ANSI
import System.IO (Handle, hPutStrLn)
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, foldM)
import Data.IORef
import Data.List (find, isPrefixOf)
import Data.Char (toLower)

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
  | Add Text (Maybe Position) Bool   -- Add entity with optional position, Bool = print after
  | Upgrade Text    -- Upgrade champion by shorthand or position (auto-consume)
  | Sell [Text]       -- Sell champions by shorthand or position
  | Find Text       -- Find entities by fuzzy search
  | Level (Maybe Int) -- Set or increment level
  | Round (Maybe (Int, Int)) -- Set stage-round
  | Money Int       -- Set gold amount
  | Odds Text (Maybe Int) Bool  -- Odds <champion> [taken] [*verbose]
  | Expected Text Int (Maybe Int) Bool  -- Expected <champion> <want> [taken] [*verbose]
  | Pool (Maybe Int) -- Pool [cost tier]
  | Unknown Text    -- Unknown command
  deriving (Show, Eq)

-- | List of all available commands with descriptions
allCommands :: [(Text, Text)]
allCommands =
  [ ("print", "Display the current game state as TSL")
  , ("add <entity>[.stars] [pos] [*]", "Add champion/item/augment at position (pos: 00-36 or B0-B8)")
  , ("upgrade <entity|pos>", "Upgrade champion (auto-consumes 2 copies)")
  , ("sell <entity|pos>...", "Sell champions by shorthand or position")
  , ("find <query>", "Search for champions/items/augments by name or shorthand")
  , ("level [n]", "Set level to n, or increment by 1 if no argument")
  , ("round [s r]", "Set stage-round to s-r, or increment if no arguments")
  , ("money <n>", "Set gold to n")
  , ("odds <champ> [taken] [*]", "Probability of hitting champion (* for verbose)")
  , ("expected <champ> <want> [taken] [*]", "Expected gold to hit <want> copies")
  , ("pool [cost]", "Show pool sizes for cost tier(s)")
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

    "add" | length parts >= 2 -> parseAddCommand (tail parts)
    "a" | length parts >= 2 -> parseAddCommand (tail parts)

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

    "odds" | length parts >= 2 -> parseOdds (tail parts)
    "o" | length parts >= 2 -> parseOdds (tail parts)

    "expected" | length parts >= 3 -> parseExpected (tail parts)
    "e" | length parts >= 3 -> parseExpected (tail parts)

    "pool" -> parsePool (tail parts)

    "" -> Unknown ""
    _ -> Unknown trimmed
  where
    -- parse add command: add <entity> [position] [*]
    parseAddCommand :: [Text] -> REPLCommand
    parseAddCommand [] = Unknown input
    parseAddCommand [entity] =
      -- single arg: could be "ani" or "ani.2" or "ani*"
      let hasAsterisk = T.isSuffixOf "*" entity
          cleanEntity = if hasAsterisk then T.dropEnd 1 entity else entity
      in Add cleanEntity Nothing hasAsterisk
    parseAddCommand [arg1, arg2] =
      -- two args: entity + position, entity + *, or position + *
      let hasAsterisk = T.isSuffixOf "*" arg2
          cleanArg2 = if hasAsterisk then T.dropEnd 1 arg2 else arg2
      in if hasAsterisk && T.null cleanArg2
         then -- "add ani *" - entity then asterisk
           Add arg1 Nothing True
         else if looksLikePosition arg2
              then -- "add ani 23" - entity then position
                Add arg1 (parsePosition arg2) False
              else if hasAsterisk && looksLikePosition cleanArg2
                   then -- "add ani 23*" - entity then position with asterisk attached
                     Add arg1 (parsePosition cleanArg2) True
                   else -- "add ani *" where * is separate
                     if arg2 == "*"
                     then Add arg1 Nothing True
                     else Add arg1 Nothing False  -- treat second arg as ignored
    parseAddCommand [entity, pos, asterisk] =
      -- three args: entity, position, *
      if asterisk == "*" || T.isSuffixOf "*" asterisk
      then Add entity (parsePosition pos) True
      else Add entity (parsePosition pos) False
    parseAddCommand (entity:rest) =
      -- more than 3 args: entity, maybe position, maybe *
      let hasAsterisk = any (\t -> t == "*" || T.isSuffixOf "*" t) rest
          maybePos = find looksLikePosition rest
      in Add entity (maybePos >>= parsePosition) hasAsterisk

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

    -- parse odds command: odds <champ> [taken] [*]
    parseOdds :: [Text] -> REPLCommand
    parseOdds [] = Unknown input
    parseOdds args =
      let hasVerbose = any (\t -> t == "*" || T.isSuffixOf "*" t) args
          cleanArgs = map (\t -> if T.isSuffixOf "*" t then T.dropEnd 1 t else t) $
                      filter (/= "*") args
          -- first arg is champion, second (if numeric) is taken count
          (champ, taken) = case cleanArgs of
            [c] -> (c, Nothing)
            [c, n] -> case reads (T.unpack n) of
              [(val, "")] -> (c, Just val)
              _ -> (c, Nothing)
            _ -> (head cleanArgs, Nothing)
      in if T.null champ
         then Unknown input
         else Odds (T.toUpper champ) taken hasVerbose

    -- parse expected command: expected <champ> <want> [taken] [*]
    parseExpected :: [Text] -> REPLCommand
    parseExpected args =
      let hasVerbose = any (\t -> t == "*" || T.isSuffixOf "*" t) args
          cleanArgs = map (\t -> if T.isSuffixOf "*" t then T.dropEnd 1 t else t) $
                      filter (/= "*") args
      in case cleanArgs of
           (c:wantStr:rest) ->
             case reads (T.unpack wantStr) of
               [(want, "")] ->
                 let taken = case rest of
                       (n:_) -> case reads (T.unpack n) of
                                  [(val, "")] -> Just val
                                  _ -> Nothing
                       _ -> Nothing
                 in Expected (T.toUpper c) want taken hasVerbose
               _ -> Unknown input
           _ -> Unknown input

    -- parse pool command: pool [cost]
    parsePool :: [Text] -> REPLCommand
    parsePool [] = Pool Nothing
    parsePool (n:_) = case reads (T.unpack n) of
      [(cost, "")] | cost >= 1 && cost <= 5 -> Pool (Just cost)
      _ -> Pool Nothing

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

  Add entity maybePos printAfter -> do
    handleAdd state entity maybePos printAfter
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

  Odds champ taken verbose -> do
    handleOdds state champ taken verbose
    return True

  Expected champ want taken verbose -> do
    handleExpected state champ want taken verbose
    return True

  Pool maybeCost -> do
    handlePool state maybeCost
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
handleAdd :: REPLState -> Text -> Maybe Position -> Bool -> IO ()
handleAdd state entity maybePos printAfter = do
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

  -- Parse entity input
  let (stars, shorthand) = parseEntityInput entity

  -- Add entity with optional position
  result <- addEntity (replGameData state) gs stars shorthand maybePos

  case result of
    Left err -> do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Error: " ++ err
      setSGR [Reset]
    Right (newState, posStr) -> do
      writeIORef (replGameStateRef state) (Just newState)
      setSGR [SetColor Foreground Vivid Green]
      putStrLn $ "Added " ++ T.unpack shorthand ++ posStr
      setSGR [Reset]

  when printAfter $ do
    putStrLn ""
    printGameState state

-- | Parse entity input to extract stars and shorthand
-- new format: SHORTHAND.stars (e.g., "ANI.2" or "ANI" for 1-star)
parseEntityInput :: Text -> (Int, Text)
parseEntityInput input =
  let stripped = T.strip input
      upperInput = T.toUpper stripped
  in if T.null upperInput
     then (1, "")
     else case T.breakOnEnd "." upperInput of
            ("", shorthand) -> (1, shorthand)  -- no dot: default 1-star
            (prefix, starsPart) ->
              let shorthand = T.dropEnd 1 prefix  -- remove trailing dot
              in case reads (T.unpack starsPart) of
                   [(stars, "")] | stars >= 1 && stars <= 3 -> (stars, shorthand)
                   _ -> (1, upperInput)  -- invalid star level, treat whole thing as shorthand

-- | Add an entity to the game state
-- returns either an error or (new state, position info string)
addEntity :: GameData -> GameState -> Int -> Text -> Maybe Position -> IO (Either String (GameState, String))
addEntity gameData gs stars shorthand maybePos = do
  let champSh = ChampionShorthand shorthand
      itemSh = ItemShorthand shorthand
      augSh = AugmentShorthand shorthand

  -- Try champion first
  case lookupChampion gameData champSh of
    Just _ -> do
      -- determine position: use explicit or find first available
      let posResult = case maybePos of
            Just pos ->
              -- check if position is valid
              if not (isValidPosition pos)
              then Left $ "Invalid position: " ++ formatPosition pos
              -- check if position is occupied
              else case getChampionAt (gsBoard gs) pos of
                Just existingChamp ->
                  let ChampionShorthand existingSh = champShorthand existingChamp
                  in Left $ "Position " ++ formatPosition pos ++ " is occupied by " ++ T.unpack existingSh
                Nothing -> Right pos
            Nothing -> case firstAvailablePosition (gsBoard gs) of
              Nothing -> Left "No available positions on board or bench"
              Just pos -> Right pos
      case posResult of
        Left err -> return $ Left err
        Right pos -> return $ Right
          ( gs { gsBoard = gsBoard gs ++ [Champion stars champSh pos] }
          , " at " ++ formatPosition pos
          )
    Nothing ->
      -- Try item (items don't have positions)
      case lookupItem gameData itemSh of
        Just _ -> return $ Right (gs { gsItems = gsItems gs ++ [itemSh] }, "")
        Nothing ->
          -- Try augment (augments don't have positions)
          case lookupAugment gameData augSh of
            Just _ ->
              if length (gsAugments gs) >= 3
              then return $ Left "Cannot add more than 3 augments"
              else return $ Right (gs { gsAugments = gsAugments gs ++ [augSh] }, "")
            Nothing -> return $ Left $ "Unknown entity: " ++ T.unpack shorthand

-- | Handle upgrade command with auto-consume
-- finds target by position or shorthand, then consumes 2 other copies of same star level
handleUpgrade :: REPLState -> Text -> IO ()
handleUpgrade state entity = do
  currentState <- readIORef (replGameStateRef state)

  case currentState of
    Nothing -> do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn "Error: No game state loaded."
      setSGR [Reset]
    Just gs -> do
      let stripped = T.strip entity

      -- find target champion by position or shorthand
      targetResult <- if looksLikePosition stripped
        then case parsePosition stripped of
          Nothing -> return $ Left $ "Invalid position: " ++ T.unpack stripped
          Just pos -> case getChampionAt (gsBoard gs) pos of
            Nothing -> return $ Left $ "No champion at position " ++ formatPosition pos
            Just champ -> return $ Right champ
        else do
          let shorthand = T.toUpper stripped
              champSh = ChampionShorthand shorthand
          case find (\c -> champShorthand c == champSh) (gsBoard gs) of
            Nothing -> return $ Left $ "Champion not found: " ++ T.unpack shorthand
            Just champ -> return $ Right champ

      case targetResult of
        Left err -> do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn $ "Error: " ++ err
          setSGR [Reset]
        Right targetChamp -> upgradeChampion gs targetChamp
  where
    upgradeChampion :: GameState -> Champion -> IO ()
    upgradeChampion gs targetChamp = do
      let targetSh = champShorthand targetChamp
          ChampionShorthand shText = targetSh
          currentStars = champStars targetChamp
          targetPos = champPosition targetChamp

      if currentStars >= 3
      then do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ "Error: " ++ T.unpack shText ++ " is already 3-star"
        setSGR [Reset]
      else do
        -- find 2 other copies with same shorthand AND same star level
        let otherCopies = filter (\c ->
              champShorthand c == targetSh &&
              champStars c == currentStars &&
              champPosition c /= targetPos) (gsBoard gs)

        if length otherCopies < 2
        then do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn $ "Error: Need 2 more " ++ show currentStars ++ "-star " ++ T.unpack shText ++
                     " copies to upgrade (found " ++ show (length otherCopies) ++ ")"
          setSGR [Reset]
        else do
          -- consume target + 2 other copies, create upgraded champion at target position
          let copiesToRemove = take 2 otherCopies
              positionsToRemove = targetPos : map champPosition copiesToRemove
              boardWithoutConsumed = filter (\c -> champPosition c `notElem` positionsToRemove) (gsBoard gs)
              upgradedChamp = Champion (currentStars + 1) targetSh targetPos
              newBoard = boardWithoutConsumed ++ [upgradedChamp]
              newState = gs { gsBoard = newBoard }

          writeIORef (replGameStateRef state) (Just newState)
          setSGR [SetColor Foreground Vivid Green]
          putStrLn $ "Upgraded " ++ T.unpack shText ++ " to " ++ show (currentStars + 1) ++
                     "-star at " ++ formatPosition targetPos ++ " (consumed 2 copies)"
          setSGR [Reset]

-- | Handle sell command (with multiple champions by shorthand or position)
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
    processChampion gs target = do
      let stripped = T.strip target

      -- check if target looks like a position
      if looksLikePosition stripped
      then case parsePosition stripped of
        Nothing -> do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn $ "Error: Invalid position: " ++ T.unpack stripped
          setSGR [Reset]
          return gs
        Just pos -> case getChampionAt (gsBoard gs) pos of
          Nothing -> do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn $ "Error: No champion at position " ++ formatPosition pos
            setSGR [Reset]
            return gs
          Just champ -> sellChampion gs champ
      else do
        -- treat as shorthand
        let shorthand = T.toUpper stripped
            champSh = ChampionShorthand shorthand
        case find (\c -> champShorthand c == champSh) (gsBoard gs) of
          Nothing -> do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn $ "Error: Champion not found: " ++ T.unpack shorthand
            setSGR [Reset]
            return gs
          Just champ -> sellChampion gs champ

    sellChampion :: GameState -> Champion -> IO GameState
    sellChampion gs champ = do
      let champSh = champShorthand champ
          ChampionShorthand shText = champSh
      case lookupChampion (replGameData state) champSh of
        Nothing -> do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn "Error: Champion data not found"
          setSGR [Reset]
          return gs
        Just champData -> do
          let cost = maybe 1 id (cdCost champData)
              champLevel = champStars champ
              goldGained = champLevel * cost - (if champLevel == 1 then 0 else 1)
              targetPos = champPosition champ
              newBoard = filter (\c -> champPosition c /= targetPos) (gsBoard gs)
              newState = gs
                { gsBoard = newBoard
                , gsGold = gsGold gs + goldGained
                }

          setSGR [SetColor Foreground Vivid Green]
          putStrLn $ "Sold " ++ T.unpack shText ++ " at " ++ formatPosition targetPos ++ " for " ++ show goldGained ++ " gold"
          setSGR [Reset]
          return newState

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

-- | Handle odds command
handleOdds :: REPLState -> Text -> Maybe Int -> Bool -> IO ()
handleOdds state champSh' maybeTaken verbose = do
  currentState <- readIORef (replGameStateRef state)
  let gameData = replGameData state
      champSh = ChampionShorthand champSh'

  case lookupChampion gameData champSh of
    Nothing -> do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Unknown champion: " ++ T.unpack champSh'
      setSGR [Reset]
    Just champData -> do
      let cost = maybe 1 id (cdCost champData)
          effectiveCost = if cost == 7 then 5 else cost
          level = maybe 1 gsLevel currentState
          taken = maybe 0 id maybeTaken
          poolSize = poolSizeForCost effectiveCost
          copiesRemaining = max 0 (poolSize - taken)
          tierTotal = tierTotalPool gameData effectiveCost
          tierRemaining = tierTotal - taken  -- simplified: only tracking this champ
          tierOdds = rollOdds level effectiveCost
          slotProb = slotProbability tierOdds copiesRemaining tierRemaining
          shopProb = shopProbability slotProb

      setSGR [SetColor Foreground Vivid Cyan]
      if verbose
        then putStr $ formatOddsVerbose (cdName champData) cost level copiesRemaining tierRemaining poolSize tierOdds slotProb shopProb
        else putStrLn $ formatOddsCompact (cdName champData) cost level copiesRemaining tierRemaining poolSize shopProb
      setSGR [Reset]

-- | Handle expected command
handleExpected :: REPLState -> Text -> Int -> Maybe Int -> Bool -> IO ()
handleExpected state champSh' want maybeTaken verbose = do
  currentState <- readIORef (replGameStateRef state)
  let gameData = replGameData state
      champSh = ChampionShorthand champSh'

  case lookupChampion gameData champSh of
    Nothing -> do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Unknown champion: " ++ T.unpack champSh'
      setSGR [Reset]
    Just champData -> do
      let cost = maybe 1 id (cdCost champData)
          effectiveCost = if cost == 7 then 5 else cost
          level = maybe 1 gsLevel currentState
          taken = maybe 0 id maybeTaken
          poolSize = poolSizeForCost effectiveCost
          copiesRemaining = max 0 (poolSize - taken)
          tierTotal = tierTotalPool gameData effectiveCost
          tierRemaining = tierTotal - taken
          tierOdds = rollOdds level effectiveCost

      if copiesRemaining < want
        then do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn $ "Impossible: only " ++ show copiesRemaining ++ " copies remaining, want " ++ show want
          setSGR [Reset]
        else do
          let expectedGold = expectedGoldToHit want cost tierOdds copiesRemaining tierRemaining poolSize

          setSGR [SetColor Foreground Vivid Cyan]
          if verbose
            then putStr $ formatExpectedVerbose (cdName champData) want cost level copiesRemaining tierRemaining poolSize tierOdds expectedGold
            else putStrLn $ formatExpectedCompact (cdName champData) want cost expectedGold
          setSGR [Reset]

-- | Handle pool command
handlePool :: REPLState -> Maybe Int -> IO ()
handlePool state maybeCost = do
  let gameData = replGameData state

  setSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]
  putStrLn "Pool Status:"
  setSGR [Reset]
  putStrLn ""

  case maybeCost of
    Nothing -> do
      -- show all tiers
      mapM_ (\c -> putStrLn $ "  " ++ formatPoolStatus gameData c) [1..5]
    Just cost -> do
      putStrLn $ "  " ++ formatPoolStatus gameData cost

  putStrLn ""

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
  let pos = champPosition champ
  let posStr = if isBenchPosition pos
               then "bench:" ++ show (posCol pos)
               else show (posRow pos) ++ "," ++ show (posCol pos)
  case lookupChampion gd shorthand of
    Nothing -> putStrLn $ "  [" ++ show stars ++ "★] " ++ show shorthand ++ " @ " ++ posStr ++ " (unknown champion)"
    Just champData -> do
      let name = T.unpack $ cdName champData
      let costStr = case cdCost champData of
            Just c -> "Cost: " ++ show c
            Nothing -> "Summoned Unit"
      let traits = T.unpack $ T.intercalate ", " (cdTraits champData)
      putStrLn $ "  [" ++ show stars ++ "★] " ++ name ++ " @ " ++ posStr ++ " (" ++ costStr ++ ")"
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
