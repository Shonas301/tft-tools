{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec
import Parser
import Types
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "parseGameState" $ do
    it "parses a simple game state" $ do
      let input = "7 3-5 32g 67h [c=2ANI] [i=BFS] [a=AA]"
      case parseGameState input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right gs -> do
          gsLevel gs `shouldBe` 7
          gsStage gs `shouldBe` 3
          gsRound gs `shouldBe` 5
          gsGold gs `shouldBe` 32
          gsHealth gs `shouldBe` 67
          length (gsBoard gs) `shouldBe` 1
          length (gsItems gs) `shouldBe` 1
          length (gsAugments gs) `shouldBe` 1

    it "parses empty board, items, and augments" $ do
      let input = "5 2-1 50g 100h [] [] []"
      case parseGameState input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right gs -> do
          gsLevel gs `shouldBe` 5
          null (gsBoard gs) `shouldBe` True
          null (gsItems gs) `shouldBe` True
          null (gsAugments gs) `shouldBe` True

    it "parses multiple champions" $ do
      let input = "9 5-2 15g 42h [c=2KOYU,c=3VEI,c=2LUX] [] []"
      case parseGameState input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right gs -> do
          length (gsBoard gs) `shouldBe` 3
          let [champ1, champ2, champ3] = gsBoard gs
          champStars champ1 `shouldBe` 2
          champStars champ2 `shouldBe` 3
          champStars champ3 `shouldBe` 2

    it "handles missing optional sections" $ do
      let input = "7 3-5 32g 67h [c=2ANI]"
      case parseGameState input of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right gs -> do
          length (gsBoard gs) `shouldBe` 1
          null (gsItems gs) `shouldBe` True
          null (gsAugments gs) `shouldBe` True

    it "rejects invalid format" $ do
      let input = "invalid format"
      case parseGameState input of
        Left _ -> return ()  -- Expected to fail
        Right _ -> expectationFailure "Should have failed to parse"
