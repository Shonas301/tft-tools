{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DataLoader
  ( GameData(..)
  , loadGameData
  , loadGameDataEmbedded
  , lookupChampion
  , lookupItem
  , lookupAugment
  ) where

import Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import Data.Csv hiding (lookup)
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.FileEmbed (embedFile)

-- | Container for all game data
data GameData = GameData
  { gdChampions :: Map ChampionShorthand ChampionData
  , gdItems :: Map ItemShorthand ItemData
  , gdAugments :: Map AugmentShorthand AugmentData
  } deriving (Show)

-- | Load all game data from CSV files
loadGameData :: FilePath -> FilePath -> FilePath -> IO (Either String GameData)
loadGameData champPath itemPath augPath = do
  champResult <- loadChampions champPath
  itemResult <- loadItems itemPath
  augResult <- loadAugments augPath

  case (champResult, itemResult, augResult) of
    (Right champs, Right items, Right augs) ->
      return $ Right $ GameData champs items augs
    (Left err, _, _) -> return $ Left $ "Error loading champions: " ++ err
    (_, Left err, _) -> return $ Left $ "Error loading items: " ++ err
    (_, _, Left err) -> return $ Left $ "Error loading augments: " ++ err

-- | Load champions from CSV
loadChampions :: FilePath -> IO (Either String (Map ChampionShorthand ChampionData))
loadChampions path = do
  csvData <- BL.readFile path
  case decodeByName csvData of
    Left err -> return $ Left err
    Right (_, rows) -> do
      let champList = V.toList rows
      let champMap = Map.fromList [(cdShorthand c, c) | c <- champList]
      return $ Right champMap

parseIntMaybe :: Text -> Maybe Int
parseIntMaybe t = case reads (T.unpack t) of
  [(n, "")] -> Just n
  _ -> Nothing

instance FromNamedRecord ChampionData where
  parseNamedRecord r = do
    name <- r .: "name"
    url <- r .: "url"
    costRaw <- r .: "cost"
    metaCostRaw <- r .: "metaCost"
    traitsRaw <- r .: "traits"
    shorthandRaw <- r .: "shorthand"
    let cost = if T.null costRaw then Nothing else parseIntMaybe costRaw
    let metaCost = if T.null metaCostRaw then Nothing else parseIntMaybe metaCostRaw
    let traits = if T.null traitsRaw then [] else T.splitOn ";" traitsRaw
    let shorthand = ChampionShorthand (T.toUpper shorthandRaw)
    return $ ChampionData name url cost metaCost (map T.strip traits) shorthand

-- | Load items from CSV
loadItems :: FilePath -> IO (Either String (Map ItemShorthand ItemData))
loadItems path = do
  csvData <- BL.readFile path
  case decodeByName csvData of
    Left err -> return $ Left err
    Right (_, rows) -> do
      let itemList = V.toList rows
      let itemMap = Map.fromList [(idShorthand i, i) | i <- itemList]
      return $ Right itemMap

instance FromNamedRecord ItemData where
  parseNamedRecord r = do
    slug <- r .: "slug"
    name <- r .: "name"
    imageUrl <- r .: "imageUrl"
    itemType <- r .: "type"
    shorthandRaw <- r .: "shorthand"
    let shorthand = ItemShorthand (T.toUpper shorthandRaw)
    return $ ItemData slug name imageUrl itemType shorthand

-- | Load augments from CSV
loadAugments :: FilePath -> IO (Either String (Map AugmentShorthand AugmentData))
loadAugments path = do
  csvData <- BL.readFile path
  case decodeByName csvData of
    Left err -> return $ Left err
    Right (_, rows) -> do
      let augList = V.toList rows
      let augMap = Map.fromList [(adShorthand a, a) | a <- augList]
      return $ Right augMap

instance FromNamedRecord AugmentData where
  parseNamedRecord r = do
    name <- r .: "name"
    icon <- r .: "icon"
    tags <- r .: "tags"
    description <- r .: "description"
    shorthandRaw <- r .: "shorthand"
    let shorthand = AugmentShorthand (T.toUpper shorthandRaw)
    return $ AugmentData name icon tags description shorthand

-- | Embedded CSV data using Template Haskell
embeddedChampionsCSV :: ByteString
embeddedChampionsCSV = $(embedFile "tft-data/set_16_champions.csv")

embeddedItemsCSV :: ByteString
embeddedItemsCSV = $(embedFile "tft-data/set_16_items.csv")

embeddedAugmentsCSV :: ByteString
embeddedAugmentsCSV = $(embedFile "tft-data/set_16_augments.csv")

-- | Load game data from embedded CSV files
-- This version loads the data compiled into the executable
loadGameDataEmbedded :: Either String GameData
loadGameDataEmbedded =
  let champResult = loadChampionsFromBS (BL.fromStrict embeddedChampionsCSV)
      itemResult = loadItemsFromBS (BL.fromStrict embeddedItemsCSV)
      augResult = loadAugmentsFromBS (BL.fromStrict embeddedAugmentsCSV)
  in case (champResult, itemResult, augResult) of
    (Right champs, Right items, Right augs) ->
      Right $ GameData champs items augs
    (Left err, _, _) -> Left $ "Error loading champions: " ++ err
    (_, Left err, _) -> Left $ "Error loading items: " ++ err
    (_, _, Left err) -> Left $ "Error loading augments: " ++ err

-- | Load champions from ByteString
loadChampionsFromBS :: BL.ByteString -> Either String (Map ChampionShorthand ChampionData)
loadChampionsFromBS csvData =
  case decodeByName csvData of
    Left err -> Left err
    Right (_, rows) -> do
      let champList = V.toList rows
      let champMap = Map.fromList [(cdShorthand c, c) | c <- champList]
      Right champMap

-- | Load items from ByteString
loadItemsFromBS :: BL.ByteString -> Either String (Map ItemShorthand ItemData)
loadItemsFromBS csvData =
  case decodeByName csvData of
    Left err -> Left err
    Right (_, rows) -> do
      let itemList = V.toList rows
      let itemMap = Map.fromList [(idShorthand i, i) | i <- itemList]
      Right itemMap

-- | Load augments from ByteString
loadAugmentsFromBS :: BL.ByteString -> Either String (Map AugmentShorthand AugmentData)
loadAugmentsFromBS csvData =
  case decodeByName csvData of
    Left err -> Left err
    Right (_, rows) -> do
      let augList = V.toList rows
      let augMap = Map.fromList [(adShorthand a, a) | a <- augList]
      Right augMap

-- Lookup functions

lookupChampion :: GameData -> ChampionShorthand -> Maybe ChampionData
lookupChampion gd sh = Map.lookup sh (gdChampions gd)

lookupItem :: GameData -> ItemShorthand -> Maybe ItemData
lookupItem gd sh = Map.lookup sh (gdItems gd)

lookupAugment :: GameData -> AugmentShorthand -> Maybe AugmentData
lookupAugment gd sh = Map.lookup sh (gdAugments gd)
