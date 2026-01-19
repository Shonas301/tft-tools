{-# LANGUAGE OverloadedStrings #-}

module ProbabilitySpec (spec) where

import Test.Hspec
import Probability

spec :: Spec
spec = do
  describe "poolSizeForCost" $ do
    it "returns correct pool sizes" $ do
      poolSizeForCost 1 `shouldBe` 30
      poolSizeForCost 2 `shouldBe` 25
      poolSizeForCost 3 `shouldBe` 18
      poolSizeForCost 4 `shouldBe` 10
      poolSizeForCost 5 `shouldBe` 9

    it "treats 7-cost as 5-cost" $ do
      poolSizeForCost 7 `shouldBe` 9

    it "returns 0 for invalid costs" $ do
      poolSizeForCost 0 `shouldBe` 0
      poolSizeForCost 6 `shouldBe` 0
      poolSizeForCost (-1) `shouldBe` 0

  describe "rollOdds" $ do
    it "returns 100% 1-cost at level 1" $ do
      rollOdds 1 1 `shouldBe` 1.0
      rollOdds 1 2 `shouldBe` 0.0
      rollOdds 1 3 `shouldBe` 0.0

    it "returns correct odds at level 7" $ do
      rollOdds 7 1 `shouldBe` 0.15
      rollOdds 7 2 `shouldBe` 0.20
      rollOdds 7 3 `shouldBe` 0.32
      rollOdds 7 4 `shouldBe` 0.30
      rollOdds 7 5 `shouldBe` 0.03

    it "returns 0 for invalid level/cost" $ do
      rollOdds 0 1 `shouldBe` 0.0
      rollOdds 11 1 `shouldBe` 0.0
      rollOdds 5 0 `shouldBe` 0.0
      rollOdds 5 6 `shouldBe` 0.0

  describe "slotProbability" $ do
    it "calculates basic slot probability" $ do
      -- at level 1, 100% 1-cost, 30 copies remaining out of 420 total
      let prob = slotProbability 1.0 30 420
      -- should be 30/420 = ~7.14%
      prob `shouldSatisfy` (\p -> abs (p - 30/420) < 0.0001)

    it "returns 0 when no copies remaining" $ do
      slotProbability 1.0 0 420 `shouldBe` 0.0

    it "returns 0 when tier empty" $ do
      slotProbability 1.0 30 0 `shouldBe` 0.0

  describe "shopProbability" $ do
    it "calculates 5-slot probability" $ do
      -- P(at least 1) = 1 - (1 - p)^5
      let slotProb = 0.10
          expected = 1 - (1 - slotProb) ** 5  -- ~0.41
      abs (shopProbability slotProb - expected) `shouldSatisfy` (< 0.0001)

    it "returns 0 for 0 slot probability" $ do
      shopProbability 0.0 `shouldBe` 0.0

    it "returns 1 for certainty" $ do
      shopProbability 1.0 `shouldBe` 1.0

  describe "expectedShopsToHit" $ do
    it "returns 0 for 0 copies wanted" $ do
      expectedShopsToHit 0 0.5 10 100 30 `shouldBe` 0.0

    it "returns infinity when impossible" $ do
      -- 0 copies remaining
      expectedShopsToHit 1 0.5 0 100 30 `shouldSatisfy` isInfinite

    it "calculates expected shops for 1 copy" $ do
      -- with high probability, should be low expected shops
      let shops = expectedShopsToHit 1 1.0 30 420 30
      shops `shouldSatisfy` (> 0)
      shops `shouldSatisfy` (< 100)  -- reasonable upper bound

  describe "expectedGoldToHit" $ do
    it "includes champion buy cost" $ do
      -- cost to hit 1 copy of a 3-cost champ
      let gold1 = expectedGoldToHit 1 3 0.5 10 100 18
      -- cost to hit 1 copy of a 1-cost champ (same odds)
      let gold2 = expectedGoldToHit 1 1 0.5 10 100 30
      -- difference should be ~2g (3g - 1g buy cost difference)
      (gold1 - gold2) `shouldSatisfy` (\d -> abs (d - 2) < 0.5)
