{-# LANGUAGE OverloadedStrings #-}

module Probability
  ( -- pool data
    poolSizeForCost
  , championsInTier
  , tierTotalPool
  , rollOdds
    -- probability calculations
  , slotProbability
  , shopProbability
  , expectedShopsToHit
  , expectedGoldToHit
    -- formatted output
  , formatOddsCompact
  , formatOddsVerbose
  , formatExpectedCompact
  , formatExpectedVerbose
  , formatPoolStatus
  ) where

import Types
import DataLoader
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Text.Printf (printf)

-- | base pool size for each cost tier
-- 1-cost = 30, 2-cost = 25, 3-cost = 18, 4-cost = 10, 5-cost = 9
-- 7-cost treated as 5-cost per user spec
poolSizeForCost :: Int -> Int
poolSizeForCost 1 = 30
poolSizeForCost 2 = 25
poolSizeForCost 3 = 18
poolSizeForCost 4 = 10
poolSizeForCost 5 = 9
poolSizeForCost 7 = 9  -- treat 7-cost as 5-cost
poolSizeForCost _ = 0  -- invalid cost

-- | roll odds table: rollOdds level cost -> probability (0-1)
-- rows = player level (1-10), cols = cost tier (1-5)
rollOdds :: Int -> Int -> Double
rollOdds level cost
  | level < 1 || level > 10 = 0
  | cost < 1 || cost > 5 = 0
  | otherwise = rollOddsTable !! (level - 1) !! (cost - 1)
  where
    -- normalized odds from roll_odds.txt
    rollOddsTable :: [[Double]]
    rollOddsTable =
      [ [1.00, 0.00, 0.00, 0.00, 0.00]  -- level 1
      , [0.75, 0.25, 0.00, 0.00, 0.00]  -- level 2
      , [0.55, 0.30, 0.15, 0.00, 0.00]  -- level 3
      , [0.45, 0.33, 0.20, 0.02, 0.00]  -- level 4
      , [0.30, 0.40, 0.25, 0.05, 0.00]  -- level 5
      , [0.19, 0.30, 0.40, 0.10, 0.01]  -- level 6
      , [0.15, 0.20, 0.32, 0.30, 0.03]  -- level 7
      , [0.10, 0.17, 0.25, 0.33, 0.15]  -- level 8
      , [0.05, 0.10, 0.20, 0.40, 0.25]  -- level 9
      , [0.01, 0.02, 0.12, 0.50, 0.35]  -- level 10
      ]

-- | count champions in a cost tier
championsInTier :: GameData -> Int -> Int
championsInTier gd cost =
  let targetCost = if cost == 7 then 5 else cost  -- 7-cost treated as 5-cost for odds
  in length $ filter (hasCost targetCost) $ M.elems (gdChampions gd)
  where
    hasCost c champ = case cdCost champ of
      Just champCost -> champCost == c || (champCost == 7 && c == 5)
      Nothing -> False

-- | total cards in a tier (champions × pool size)
tierTotalPool :: GameData -> Int -> Int
tierTotalPool gd cost =
  let effectiveCost = if cost == 7 then 5 else cost
      numChamps = championsInTier gd effectiveCost
      poolSize = poolSizeForCost effectiveCost
  in numChamps * poolSize

-- | probability of hitting specific champion on a single shop slot
-- tierOdds = rollOdds[level][cost]
-- copiesRemaining = poolSize - taken
-- tierRemaining = total cards in tier - all taken from tier (simplified: we only track target champ)
slotProbability :: Double -> Int -> Int -> Double
slotProbability tierOdds copiesRemaining tierRemaining
  | tierRemaining <= 0 = 0
  | copiesRemaining <= 0 = 0
  | otherwise = tierOdds * (fromIntegral copiesRemaining / fromIntegral tierRemaining)

-- | probability of hitting at least 1 copy in a shop (5 slots)
shopProbability :: Double -> Double
shopProbability slotProb
  | slotProb <= 0 = 0
  | slotProb >= 1 = 1
  | otherwise = 1 - (1 - slotProb) ** 5

-- | expected number of shops to hit n copies
-- accounts for probability shift as copies are found
expectedShopsToHit :: Int -> Double -> Int -> Int -> Int -> Double
expectedShopsToHit 0 _ _ _ _ = 0
expectedShopsToHit n tierOdds copiesRemaining tierRemaining poolSize
  | copiesRemaining <= 0 = 1/0  -- infinity - impossible
  | otherwise =
      let slotP = slotProbability tierOdds copiesRemaining tierRemaining
          shopP = shopProbability slotP
          expectedForOne = if shopP > 0 then 1 / shopP else 1/0
          -- after finding one, pool shrinks by 1
          remaining = expectedShopsToHit (n - 1) tierOdds (copiesRemaining - 1) (tierRemaining - 1) poolSize
      in expectedForOne + remaining

-- | expected gold to hit n copies (2g per shop + n × champion cost)
expectedGoldToHit :: Int -> Int -> Double -> Int -> Int -> Int -> Double
expectedGoldToHit n champCost tierOdds copiesRemaining tierRemaining poolSize =
  let shops = expectedShopsToHit n tierOdds copiesRemaining tierRemaining poolSize
      rollCost = 2 * shops
      buyCost = fromIntegral (n * champCost)
  in rollCost + buyCost

-- | format odds output (compact)
formatOddsCompact :: Text -> Int -> Int -> Int -> Int -> Int -> Double -> String
formatOddsCompact name cost level copiesRemaining tierRemaining _ shopProb =
  printf "%s (%d-cost): %d/%d remaining → %.1f%% per shop (L%d)"
    (T.unpack name) cost copiesRemaining tierRemaining (shopProb * 100) level

-- | format odds output (verbose)
formatOddsVerbose :: Text -> Int -> Int -> Int -> Int -> Int -> Double -> Double -> Double -> String
formatOddsVerbose name cost level copiesRemaining tierRemaining poolSize tierOdds slotProb shopProb =
  unlines
    [ printf "%s (%d-cost): %d/%d remaining" (T.unpack name) cost copiesRemaining tierRemaining
    , printf "  Base pool: %d copies each" poolSize
    , printf "  Tier odds at L%d: %.0f%%" level (tierOdds * 100)
    , printf "  Slot probability: %.2f%%" (slotProb * 100)
    , printf "  Shop probability (5 slots): %.1f%%" (shopProb * 100)
    ]

-- | format expected cost output (compact)
formatExpectedCompact :: Text -> Int -> Int -> Double -> String
formatExpectedCompact name want champCost expectedGold =
  let shops = (expectedGold - fromIntegral (want * champCost)) / 2
      suffix :: String
      suffix = if want == 1 then "y" else "ies"
  in printf "%s: ~%.0fg to hit %d cop%s (%.0f shops avg)"
       (T.unpack name) expectedGold want suffix shops

-- | format expected cost output (verbose)
formatExpectedVerbose :: Text -> Int -> Int -> Int -> Int -> Int -> Int -> Double -> Double -> String
formatExpectedVerbose name want champCost level copiesRemaining tierRemaining poolSize tierOdds expectedGold =
  let shops = (expectedGold - fromIntegral (want * champCost)) / 2
      buyCost = want * champCost
      suffix :: String
      suffix = if want == 1 then "y" else "ies"
      _ = poolSize  -- unused but part of API for consistency
  in unlines
    [ printf "%s (%d-cost): hitting %d cop%s" (T.unpack name) champCost want suffix
    , printf "  Pool remaining: %d/%d" copiesRemaining tierRemaining
    , printf "  Tier odds at L%d: %.0f%%" level (tierOdds * 100)
    , printf "  Expected shops: %.1f" shops
    , printf "  Roll cost: ~%.0fg" (shops * 2)
    , printf "  Buy cost: %dg (%d × %dg)" buyCost want champCost
    , printf "  Total expected: ~%.0fg" expectedGold
    ]

-- | format pool status for a cost tier
formatPoolStatus :: GameData -> Int -> String
formatPoolStatus gd cost =
  let effectiveCost = if cost == 7 then 5 else cost
      numChamps = championsInTier gd effectiveCost
      poolSize = poolSizeForCost effectiveCost
      total = numChamps * poolSize
  in printf "%d-cost: %d champions × %d copies = %d total" effectiveCost numChamps poolSize total
