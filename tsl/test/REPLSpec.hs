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

    it "parses 'add' command" $ do
      parseCommand "add ani" `shouldBe` Add "ani" False
      parseCommand "a ani" `shouldBe` Add "ani" False
      parseCommand "add 2ani" `shouldBe` Add "2ani" False
      parseCommand "add ani *" `shouldBe` Add "ani" True
      parseCommand "add 2ani *" `shouldBe` Add "2ani" True

    it "parses 'upgrade' command" $ do
      parseCommand "upgrade ani" `shouldBe` Upgrade "ani"
      parseCommand "u ani" `shouldBe` Upgrade "ani"
      parseCommand "upgrade ie" `shouldBe` Upgrade "ie"

    it "parses 'sell' command" $ do
      parseCommand "sell ani" `shouldBe` Sell "ani"
      parseCommand "s ani" `shouldBe` Sell "ani"
      parseCommand "sell 2ani" `shouldBe` Sell "2ani"

    it "parses 'find' command" $ do
      parseCommand "find ani" `shouldBe` Find "ani"
      parseCommand "f ani" `shouldBe` Find "ani"
      parseCommand "find infinity edge" `shouldBe` Find "infinity edge"

  describe "parseEntityInput" $ do
    it "parses entity without star level" $ do
      parseEntityInput "ANI" `shouldBe` (1, "ANI")
      parseEntityInput "ani" `shouldBe` (1, "ANI")

    it "parses entity with star level" $ do
      parseEntityInput "2ANI" `shouldBe` (2, "ANI")
      parseEntityInput "3ani" `shouldBe` (3, "ANI")
      parseEntityInput "1IE" `shouldBe` (1, "IE")

    it "handles whitespace" $ do
      parseEntityInput "  ani  " `shouldBe` (1, "ANI")
      parseEntityInput "  2ani  " `shouldBe` (2, "ANI")

  describe "allCommands" $ do
    it "includes all basic commands" $ do
      let commandNames = map fst allCommands
      commandNames `shouldContain` ["print"]
      commandNames `shouldContain` ["help"]
      commandNames `shouldContain` ["quit"]
      commandNames `shouldContain` ["add <entity> [*]"]
      commandNames `shouldContain` ["upgrade <entity>"]
      commandNames `shouldContain` ["sell <champion>"]
      commandNames `shouldContain` ["find <query>"]
