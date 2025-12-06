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

-- | Parse board section: [c=2ANI,c=1BLI,...] or []
boardParser :: Parser [Champion]
boardParser = between (char '[') (char ']') $ do
  sepBy championParser (char ',')

-- | Parse a single champion: c=2ANI
championParser :: Parser Champion
championParser = do
  _ <- string "c="
  stars <- intParser
  shorthand <- championShorthandParser
  return $ Champion stars shorthand

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
