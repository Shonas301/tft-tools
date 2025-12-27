{-# LANGUAGE OverloadedStrings #-}

module Encoder
  ( encodeGameState
  , encodeChampion
  , encodeItem
  , encodeAugment
  , encodeBoardFen
  ) where

import Types
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)

-- | Encode a complete game state to TSL format
encodeGameState :: GameState -> Text
encodeGameState gs = T.pack $ unwords
  [ show (gsLevel gs)
  , show (gsStage gs) ++ "-" ++ show (gsRound gs)
  , show (gsGold gs) ++ "g"
  , show (gsHealth gs) ++ "h"
  , encodeBoardFen (gsBoard gs)
  , encodeItems (gsItems gs)
  , encodeAugments (gsAugments gs)
  ]

-- | Encode board section using FEN-like notation
-- format: [row0/row1/row2/row3/bench]
encodeBoardFen :: [Champion] -> String
encodeBoardFen champs =
  let rows = [encodeRow champs r (maxColsForRow r) | r <- [0..benchRow]]
  in "[" ++ intercalate "/" rows ++ "]"

-- | Encode a single row
encodeRow :: [Champion] -> Int -> Int -> String
encodeRow champs rowNum maxCols =
  let rowChamps = filter (\c -> posRow (champPosition c) == rowNum) champs
      sortedChamps = sortBy (comparing (posCol . champPosition)) rowChamps
      cells = encodeCells sortedChamps 0 maxCols
  in intercalate "|" cells

-- | Encode cells in a row, tracking empty spaces
encodeCells :: [Champion] -> Int -> Int -> [String]
encodeCells [] col maxCols
  | col >= maxCols = []
  | otherwise = [show (maxCols - col)]
encodeCells (c:rest) col maxCols =
  let champCol = posCol (champPosition c)
      emptyBefore = champCol - col
      champStr = encodeChampion c
  in if emptyBefore > 0
     then show emptyBefore : champStr : encodeCells rest (champCol + 1) maxCols
     else champStr : encodeCells rest (champCol + 1) maxCols

-- | Encode a single champion (shorthand.stars, no position)
encodeChampion :: Champion -> String
encodeChampion (Champion stars (ChampionShorthand sh) _) =
  T.unpack sh ++ "." ++ show stars

-- | Encode items section
encodeItems :: [ItemShorthand] -> String
encodeItems [] = "[]"
encodeItems items = "[" ++ intercalate "," (map encodeItem items) ++ "]"

-- | Encode a single item
encodeItem :: ItemShorthand -> String
encodeItem (ItemShorthand sh) = "i=" ++ T.unpack sh

-- | Encode augments section
encodeAugments :: [AugmentShorthand] -> String
encodeAugments [] = "[]"
encodeAugments augs = "[" ++ intercalate "," (map encodeAugment augs) ++ "]"

-- | Encode a single augment
encodeAugment :: AugmentShorthand -> String
encodeAugment (AugmentShorthand sh) = "a=" ++ T.unpack sh
