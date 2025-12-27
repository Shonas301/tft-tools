{-# LANGUAGE OverloadedStrings #-}

module Parser (parseGameState, TFTParseError) where

import Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text
type TFTParseError = ParseErrorBundle Text Void

-- | Parse a complete game state from DSL format
parseGameState :: Text -> Either TFTParseError GameState
parseGameState = parse gameStateParser "TFT-DSL"

-- | Main parser for game state
gameStateParser :: Parser GameState
gameStateParser = do
  level <- intParser <* space1
  (stage, roundNum) <- stageRoundParser <* space1
  gold <- intParser <* char 'g' <* space1
  health <- intParser <* char 'h'
  space
  board <- option [] (try boardParser <* space)
  items <- option [] (try itemsParser <* space)
  augments <- option [] augmentsParser
  eof
  return $ GameState level stage roundNum gold health board items augments

-- | Parse an integer
intParser :: Parser Int
intParser = L.decimal

-- | Parse stage-round format (e.g., "3-2")
stageRoundParser :: Parser (Int, Int)
stageRoundParser = do
  stage <- intParser
  _ <- char '-'
  roundNum <- intParser
  return (stage, roundNum)

-- | Parse board section - supports both new FEN format and legacy format
-- new format: [ANI.1|1|BLI.2|4/7/7/7/9]
-- legacy format: [c=ANI.2,c=BLI.1,...]
boardParser :: Parser [Champion]
boardParser = between (char '[') (char ']') $
  try fenBoardParser <|> legacyBoardParser

-- | Parse FEN-like board notation
-- format: row0/row1/row2/row3/bench where each row contains cells separated by |
fenBoardParser :: Parser [Champion]
fenBoardParser = do
  rowStrings <- sepBy1 fenRowStringParser (char '/')
  -- validate we have 5 rows (4 board + 1 bench)
  if length rowStrings /= 5
    then fail $ "Expected 5 rows (4 board + 1 bench), got " ++ show (length rowStrings)
    else return $ assignFenPositions rowStrings

-- | Parse a single FEN row as list of (isChamp, stars, shorthand, emptyCount)
fenRowStringParser :: Parser [FenCell]
fenRowStringParser = sepBy1 fenCellParser (char '|')

-- | Intermediate representation for FEN cells
data FenCell
  = FenChamp Int ChampionShorthand  -- stars, shorthand
  | FenEmpty Int                     -- count of empty cells

-- | Parse a single FEN cell - either a champion or empty count
fenCellParser :: Parser FenCell
fenCellParser = try fenChampionCell <|> fenEmptyCell

-- | Parse a champion cell: <SHORTHAND>.<stars>
fenChampionCell :: Parser FenCell
fenChampionCell = do
  shorthand <- championShorthandParser
  _ <- char '.'
  stars <- intParser
  return $ FenChamp stars shorthand

-- | Parse empty cells: a number 1-9
fenEmptyCell :: Parser FenCell
fenEmptyCell = do
  emptyCount <- intParser
  return $ FenEmpty emptyCount

-- | Convert parsed FEN rows to positioned champions
assignFenPositions :: [[FenCell]] -> [Champion]
assignFenPositions rows = concatMap assignRowPositions (zip [0..] rows)

-- | Assign positions within a single row
assignRowPositions :: (Int, [FenCell]) -> [Champion]
assignRowPositions (rowNum, cells) = go 0 cells
  where
    go _ [] = []
    go col (FenEmpty n : rest) = go (col + n) rest
    go col (FenChamp stars sh : rest) =
      Champion stars sh (Position rowNum col) : go (col + 1) rest

-- | Legacy board parser: [c=2ANI,c=1BLI,...]
legacyBoardParser :: Parser [Champion]
legacyBoardParser = do
  champData <- sepBy legacyChampionParser (char ',')
  -- assign positions to champions in order (left-to-right, top-to-bottom)
  return $ assignPositions champData allBoardPositions

-- | Parse a legacy champion: c=ANI.2 (returns stars and shorthand)
legacyChampionParser :: Parser (Int, ChampionShorthand)
legacyChampionParser = do
  _ <- string "c="
  shorthand <- championShorthandParser
  _ <- char '.'
  stars <- intParser
  return (stars, shorthand)

-- | Assign positions to champions in order
assignPositions :: [(Int, ChampionShorthand)] -> [Position] -> [Champion]
assignPositions [] _ = []
assignPositions _ [] = []
assignPositions ((stars, sh):rest) (pos:positions) =
  Champion stars sh pos : assignPositions rest positions

-- | Parse champion shorthand (2-4 uppercase alphanumeric chars)
championShorthandParser :: Parser ChampionShorthand
championShorthandParser = do
  code <- takeWhile1P (Just "champion shorthand") isShorthandChar
  return $ ChampionShorthand (T.toUpper code)
  where
    isShorthandChar c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

-- | Parse items section: [i=IE,i=RB,...] or []
itemsParser :: Parser [ItemShorthand]
itemsParser = between (char '[') (char ']') $ do
  sepBy itemParser (char ',')

-- | Parse a single item: i=IE
itemParser :: Parser ItemShorthand
itemParser = do
  _ <- string "i="
  code <- takeWhile1P (Just "item shorthand") isShorthandChar
  return $ ItemShorthand (T.toUpper code)
  where
    isShorthandChar c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

-- | Parse augments section: [a=HG,a=CB,...] or []
augmentsParser :: Parser [AugmentShorthand]
augmentsParser = between (char '[') (char ']') $ do
  sepBy augmentParser (char ',')

-- | Parse a single augment: a=HG
augmentParser :: Parser AugmentShorthand
augmentParser = do
  _ <- string "a="
  code <- takeWhile1P (Just "augment shorthand") isShorthandChar
  return $ AugmentShorthand (T.toUpper code)
  where
    isShorthandChar c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
