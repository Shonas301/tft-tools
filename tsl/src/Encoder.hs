{-# LANGUAGE OverloadedStrings #-}

module Encoder
  ( encodeGameState
  , encodeChampion
  , encodeItem
  , encodeAugment
  ) where

import Types
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (intercalate)

-- | Encode a complete game state to TSL format
encodeGameState :: GameState -> Text
encodeGameState gs = T.pack $ unwords
  [ show (gsLevel gs)
  , show (gsStage gs) ++ "-" ++ show (gsRound gs)
  , show (gsGold gs) ++ "g"
  , show (gsHealth gs) ++ "h"
  , encodeBoard (gsBoard gs)
  , encodeItems (gsItems gs)
  , encodeAugments (gsAugments gs)
  ]

-- | Encode board section
encodeBoard :: [Champion] -> String
encodeBoard [] = "[]"
encodeBoard champs = "[" ++ intercalate "," (map encodeChampion champs) ++ "]"

-- | Encode a single champion
encodeChampion :: Champion -> String
encodeChampion (Champion stars (ChampionShorthand sh)) =
  "c=" ++ show stars ++ T.unpack sh

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
