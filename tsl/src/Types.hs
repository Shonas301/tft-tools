module Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isDigit, digitToInt, toUpper)

-- | board constants
boardRows :: Int
boardRows = 4

boardCols :: Int
boardCols = 7

benchCols :: Int
benchCols = 9

benchRow :: Int
benchRow = 4  -- bench is represented as row 4

-- | position on the board or bench
-- row 0-3 = board rows, row 4 = bench
-- col 0-6 for board, col 0-8 for bench
data Position = Position
  { posRow :: Int
  , posCol :: Int
  } deriving (Show, Eq, Ord)

-- | check if position is on the bench
isBenchPosition :: Position -> Bool
isBenchPosition pos = posRow pos == benchRow

-- | check if position is valid
isValidPosition :: Position -> Bool
isValidPosition (Position row col)
  | row < 0 || row > benchRow = False
  | row == benchRow = col >= 0 && col < benchCols
  | otherwise = col >= 0 && col < boardCols

-- | get all board positions (left-to-right, top-to-bottom)
allBoardPositions :: [Position]
allBoardPositions = [Position r c | r <- [0..boardRows-1], c <- [0..boardCols-1]]

-- | get all bench positions
allBenchPositions :: [Position]
allBenchPositions = [Position benchRow c | c <- [0..benchCols-1]]

-- | get all positions (board first, then bench)
allPositions :: [Position]
allPositions = allBoardPositions ++ allBenchPositions

-- | find first available position not occupied by any champion
firstAvailablePosition :: [Champion] -> Maybe Position
firstAvailablePosition champs =
  let occupied = map champPosition champs
  in case filter (`notElem` occupied) allBoardPositions of
       (p:_) -> Just p
       [] -> case filter (`notElem` occupied) allBenchPositions of
               (p:_) -> Just p
               [] -> Nothing

-- | get champion at position
getChampionAt :: [Champion] -> Position -> Maybe Champion
getChampionAt champs pos =
  case filter (\c -> champPosition c == pos) champs of
    (c:_) -> Just c
    [] -> Nothing

-- | max columns for a given row
maxColsForRow :: Int -> Int
maxColsForRow row = if row == benchRow then benchCols else boardCols

-- | parse a position string
-- board: "RC" where R=row(0-3), C=col(0-6), e.g., "23" = Position 2 3
-- bench: "B0" through "B8", e.g., "B5" = Position 4 5
parsePosition :: Text -> Maybe Position
parsePosition input =
  let s = T.map toUpper $ T.strip input
  in if T.null s
     then Nothing
     else if T.length s == 2 && T.head s == 'B'
          then -- bench position: B0-B8
            let colChar = T.index s 1
            in if isDigit colChar
               then let col = digitToInt colChar
                    in if col >= 0 && col < benchCols
                       then Just (Position benchRow col)
                       else Nothing
               else Nothing
          else if T.length s == 2 && isDigit (T.head s) && isDigit (T.index s 1)
               then -- board position: RC format
                 let row = digitToInt (T.head s)
                     col = digitToInt (T.index s 1)
                 in if row >= 0 && row < boardRows && col >= 0 && col < boardCols
                    then Just (Position row col)
                    else Nothing
               else Nothing

-- | check if a text looks like a position (for distinguishing from shorthands)
looksLikePosition :: Text -> Bool
looksLikePosition input =
  let s = T.map toUpper $ T.strip input
  in (T.length s == 2 && T.head s == 'B' && isDigit (T.index s 1)) ||
     (T.length s == 2 && isDigit (T.head s) && isDigit (T.index s 1))

-- | format a position for display
formatPosition :: Position -> String
formatPosition (Position row col)
  | row == benchRow = "B" ++ show col
  | otherwise = show row ++ show col

-- | Represents a TFT game state
data GameState = GameState
  { gsLevel :: Int
  , gsStage :: Int
  , gsRound :: Int
  , gsGold :: Int
  , gsHealth :: Int
  , gsBoard :: [Champion]  -- positioned champions (board + bench)
  , gsItems :: [ItemShorthand]
  , gsAugments :: [AugmentShorthand]
  } deriving (Show, Eq)

-- | A champion on the board or bench
data Champion = Champion
  { champStars :: Int
  , champShorthand :: ChampionShorthand
  , champPosition :: Position  -- position on board (row 0-3) or bench (row 4)
  } deriving (Show, Eq)

-- | Shorthand codes for game entities
newtype ChampionShorthand = ChampionShorthand Text deriving (Show, Eq, Ord)
newtype ItemShorthand = ItemShorthand Text deriving (Show, Eq, Ord)
newtype AugmentShorthand = AugmentShorthand Text deriving (Show, Eq, Ord)

-- | Champion data from CSV
data ChampionData = ChampionData
  { cdName :: Text
  , cdUrl :: Text
  , cdCost :: Maybe Int
  , cdMetaCost :: Maybe Int
  , cdTraits :: [Text]
  , cdShorthand :: ChampionShorthand
  } deriving (Show, Eq)

-- | Item data from CSV
data ItemData = ItemData
  { idSlug :: Text
  , idName :: Text
  , idImageUrl :: Text
  , idType :: Text
  , idShorthand :: ItemShorthand
  } deriving (Show, Eq)

-- | Augment data from CSV
data AugmentData = AugmentData
  { adName :: Text
  , adIcon :: Text
  , adTags :: Text
  , adDescription :: Text
  , adShorthand :: AugmentShorthand
  } deriving (Show, Eq)
