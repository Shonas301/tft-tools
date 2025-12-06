{-# LANGUAGE OverloadedStrings #-}

module REPLSpec (spec) where

import Test.Hspec
import REPL
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "parseCommand" $ do
    it "parses 'print' command" $ do
      parseCommand "print" `shouldBe` Print
      parseCommand "p" `shouldBe` Print

    it "parses 'help' command" $ do
      parseCommand "help" `shouldBe` Help
      parseCommand "h" `shouldBe` Help
      parseCommand "?" `shouldBe` Help

    it "parses 'quit' command" $ do
      parseCommand "quit" `shouldBe` Quit
      parseCommand "exit" `shouldBe` Quit
      parseCommand "q" `shouldBe` Quit

    it "handles unknown commands" $ do
      parseCommand "unknown" `shouldBe` Unknown "unknown"
      parseCommand "foo bar" `shouldBe` Unknown "foo bar"

    it "handles empty input" $ do
      parseCommand "" `shouldBe` Unknown ""
      parseCommand "   " `shouldBe` Unknown ""

    it "is case-insensitive" $ do
      parseCommand "PRINT" `shouldBe` Print
      parseCommand "Help" `shouldBe` Help
      parseCommand "QUIT" `shouldBe` Quit

  describe "allCommands" $ do
    it "includes all basic commands" $ do
      let commandNames = map fst allCommands
      commandNames `shouldContain` ["print"]
      commandNames `shouldContain` ["help"]
      commandNames `shouldContain` ["quit"]
