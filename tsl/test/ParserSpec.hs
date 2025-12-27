{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec
import Parser
import Encoder
import Types

spec :: Spec
spec = do
  describe "parseGameState" $ do
    describe "legacy format (backward compatibility)" $ do
      it "parses a simple game state" $ do
        let input = "7 3-5 32g 67h [c=ANI.2] [i=BFS] [a=AA]"
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
        let input = "9 5-2 15g 42h [c=KOYU.2,c=VEI.3,c=LUX.2] [] []"
        case parseGameState input of
          Left err -> expectationFailure $ "Parse failed: " ++ show err
          Right gs -> do
            length (gsBoard gs) `shouldBe` 3
            let [champ1, champ2, champ3] = gsBoard gs
            champStars champ1 `shouldBe` 2
            champStars champ2 `shouldBe` 3
            champStars champ3 `shouldBe` 2
            -- legacy format assigns positions left-to-right, top-to-bottom
            champPosition champ1 `shouldBe` Position 0 0
            champPosition champ2 `shouldBe` Position 0 1
            champPosition champ3 `shouldBe` Position 0 2

      it "handles missing optional sections" $ do
        let input = "7 3-5 32g 67h [c=ANI.2]"
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

    describe "FEN format (position encoding)" $ do
      it "parses empty board with FEN notation" $ do
        let input = "5 2-1 50g 100h [7/7/7/7/9] [] []"
        case parseGameState input of
          Left err -> expectationFailure $ "Parse failed: " ++ show err
          Right gs -> do
            null (gsBoard gs) `shouldBe` True

      it "parses champion at specific position" $ do
        let input = "5 2-1 50g 100h [ANI.2|6/7/7/7/9] [] []"
        case parseGameState input of
          Left err -> expectationFailure $ "Parse failed: " ++ show err
          Right gs -> do
            length (gsBoard gs) `shouldBe` 1
            let [champ] = gsBoard gs
            champStars champ `shouldBe` 2
            champShorthand champ `shouldBe` ChampionShorthand "ANI"
            champPosition champ `shouldBe` Position 0 0

      it "parses champion with empty cells before it" $ do
        let input = "5 2-1 50g 100h [2|ANI.1|4/7/7/7/9] [] []"
        case parseGameState input of
          Left err -> expectationFailure $ "Parse failed: " ++ show err
          Right gs -> do
            length (gsBoard gs) `shouldBe` 1
            let [champ] = gsBoard gs
            champPosition champ `shouldBe` Position 0 2

      it "parses multiple champions in different rows" $ do
        let input = "5 2-1 50g 100h [ANI.1|6/BLI.2|6/7/7/9] [] []"
        case parseGameState input of
          Left err -> expectationFailure $ "Parse failed: " ++ show err
          Right gs -> do
            length (gsBoard gs) `shouldBe` 2
            let [champ1, champ2] = gsBoard gs
            champPosition champ1 `shouldBe` Position 0 0
            champPosition champ2 `shouldBe` Position 1 0

      it "parses champion on bench (row 4)" $ do
        let input = "5 2-1 50g 100h [7/7/7/7/ANI.3|8] [] []"
        case parseGameState input of
          Left err -> expectationFailure $ "Parse failed: " ++ show err
          Right gs -> do
            length (gsBoard gs) `shouldBe` 1
            let [champ] = gsBoard gs
            champPosition champ `shouldBe` Position 4 0
            isBenchPosition (champPosition champ) `shouldBe` True

      it "parses complex board layout" $ do
        -- 2 champions in row 0, 1 in row 2, 1 on bench
        let input = "7 3-5 32g 67h [ANI.1|1|BLI.2|4/7/3|LUX.1|3/7/2|SEJ.1|6] [i=BFS] [a=AA]"
        case parseGameState input of
          Left err -> expectationFailure $ "Parse failed: " ++ show err
          Right gs -> do
            length (gsBoard gs) `shouldBe` 4
            let champs = gsBoard gs
            -- check all positions
            champPosition (champs !! 0) `shouldBe` Position 0 0
            champPosition (champs !! 1) `shouldBe` Position 0 2
            champPosition (champs !! 2) `shouldBe` Position 2 3
            champPosition (champs !! 3) `shouldBe` Position 4 2

  describe "Encoder" $ do
    it "encodes empty board to FEN notation" $ do
      let gs = GameState 5 2 1 50 100 [] [] []
      encodeBoardFen (gsBoard gs) `shouldBe` "[7/7/7/7/9]"

    it "encodes champion at position 0,0" $ do
      let champ = Champion 2 (ChampionShorthand "ANI") (Position 0 0)
          board = [champ]
      encodeBoardFen board `shouldBe` "[ANI.2|6/7/7/7/9]"

    it "encodes champion with empty cells before" $ do
      let champ = Champion 1 (ChampionShorthand "ANI") (Position 0 3)
          board = [champ]
      encodeBoardFen board `shouldBe` "[3|ANI.1|3/7/7/7/9]"

    it "encodes multiple champions in a row" $ do
      let champs = [ Champion 1 (ChampionShorthand "ANI") (Position 0 0)
                   , Champion 2 (ChampionShorthand "BLI") (Position 0 3)
                   ]
      encodeBoardFen champs `shouldBe` "[ANI.1|2|BLI.2|3/7/7/7/9]"

    it "encodes champion on bench" $ do
      let champ = Champion 3 (ChampionShorthand "SEJ") (Position 4 5)
          board = [champ]
      encodeBoardFen board `shouldBe` "[7/7/7/7/5|SEJ.3|3]"

  describe "round-trip (parse -> encode -> parse)" $ do
    it "preserves game state through round-trip" $ do
      let input = "7 3-5 32g 67h [ANI.1|1|BLI.2|4/7/7/7/9] [i=BFS] [a=AA]"
      case parseGameState input of
        Left err -> expectationFailure $ "First parse failed: " ++ show err
        Right gs1 -> do
          let encoded = encodeGameState gs1
          case parseGameState encoded of
            Left err -> expectationFailure $ "Second parse failed: " ++ show err
            Right gs2 -> do
              gsLevel gs2 `shouldBe` gsLevel gs1
              gsStage gs2 `shouldBe` gsStage gs1
              gsRound gs2 `shouldBe` gsRound gs1
              gsGold gs2 `shouldBe` gsGold gs1
              gsHealth gs2 `shouldBe` gsHealth gs1
              length (gsBoard gs2) `shouldBe` length (gsBoard gs1)
              gsItems gs2 `shouldBe` gsItems gs1
              gsAugments gs2 `shouldBe` gsAugments gs1
