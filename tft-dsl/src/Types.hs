module Types where

import Data.Text (Text)

-- | Represents a TFT game state
data GameState = GameState
  { gsLevel :: Int
  , gsStage :: Int
  , gsRound :: Int
  , gsGold :: Int
  , gsHealth :: Int
  , gsBoard :: [Champion]
  , gsItems :: [ItemShorthand]
  , gsAugments :: [AugmentShorthand]
  } deriving (Show, Eq)

-- | A champion on the board
data Champion = Champion
  { champStars :: Int
  , champShorthand :: ChampionShorthand
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
