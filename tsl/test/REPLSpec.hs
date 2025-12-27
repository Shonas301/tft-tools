{-# LANGUAGE OverloadedStrings #-}

module REPLSpec (spec) where

import Test.Hspec
import REPL
import Types (Position(..), parsePosition, looksLikePosition, formatPosition)

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

    it "parses 'add' command with single entity" $ do
      parseCommand "add ani" `shouldBe` Add "ani" Nothing False
      parseCommand "a ani" `shouldBe` Add "ani" Nothing False
      parseCommand "add ani.2" `shouldBe` Add "ani.2" Nothing False
      parseCommand "add ani *" `shouldBe` Add "ani" Nothing True
      parseCommand "add ani.2 *" `shouldBe` Add "ani.2" Nothing True

    it "parses 'add' command with position" $ do
      parseCommand "add ani 23" `shouldBe` Add "ani" (Just (Position 2 3)) False
      parseCommand "add ani.2 00" `shouldBe` Add "ani.2" (Just (Position 0 0)) False
      parseCommand "add ani B5" `shouldBe` Add "ani" (Just (Position 4 5)) False
      parseCommand "add ani 23 *" `shouldBe` Add "ani" (Just (Position 2 3)) True
      parseCommand "a ani.3 B0 *" `shouldBe` Add "ani.3" (Just (Position 4 0)) True

    it "parses 'upgrade' command" $ do
      parseCommand "upgrade ani" `shouldBe` Upgrade "ani"
      parseCommand "u ani" `shouldBe` Upgrade "ani"
      parseCommand "upgrade 23" `shouldBe` Upgrade "23"
      parseCommand "u B5" `shouldBe` Upgrade "B5"

    it "parses 'sell' command with single champion" $ do
      parseCommand "sell ani" `shouldBe` Sell ["ani"]
      parseCommand "s ani" `shouldBe` Sell ["ani"]
      parseCommand "sell 23" `shouldBe` Sell ["23"]
      parseCommand "s B5" `shouldBe` Sell ["B5"]

    it "parses 'sell' command with multiple champions" $ do
      parseCommand "sell ani blit" `shouldBe` Sell ["ani", "blit"]
      parseCommand "s ani 23 B5" `shouldBe` Sell ["ani", "23", "B5"]
      parseCommand "sell 00 11 22" `shouldBe` Sell ["00", "11", "22"]

    it "parses 'find' command" $ do
      parseCommand "find ani" `shouldBe` Find "ani"
      parseCommand "f ani" `shouldBe` Find "ani"
      parseCommand "find infinity edge" `shouldBe` Find "infinity edge"

  describe "parseEntityInput" $ do
    it "parses entity without star level (default 1-star)" $ do
      parseEntityInput "ANI" `shouldBe` (1, "ANI")
      parseEntityInput "ani" `shouldBe` (1, "ANI")

    it "parses entity with star level (new SHORTHAND.stars format)" $ do
      parseEntityInput "ANI.2" `shouldBe` (2, "ANI")
      parseEntityInput "ani.3" `shouldBe` (3, "ANI")
      parseEntityInput "IE.1" `shouldBe` (1, "IE")

    it "handles whitespace" $ do
      parseEntityInput "  ani  " `shouldBe` (1, "ANI")
      parseEntityInput "  ani.2  " `shouldBe` (2, "ANI")

    it "treats invalid star level as shorthand" $ do
      parseEntityInput "ANI.5" `shouldBe` (1, "ANI.5")
      parseEntityInput "ANI.0" `shouldBe` (1, "ANI.0")

  describe "Position parsing" $ do
    it "parses board positions" $ do
      parsePosition "00" `shouldBe` Just (Position 0 0)
      parsePosition "23" `shouldBe` Just (Position 2 3)
      parsePosition "36" `shouldBe` Just (Position 3 6)

    it "parses bench positions" $ do
      parsePosition "B0" `shouldBe` Just (Position 4 0)
      parsePosition "b5" `shouldBe` Just (Position 4 5)
      parsePosition "B8" `shouldBe` Just (Position 4 8)

    it "rejects invalid positions" $ do
      parsePosition "40" `shouldBe` Nothing  -- row 4 is bench, needs B prefix
      parsePosition "37" `shouldBe` Nothing  -- col 7 out of range for board
      parsePosition "B9" `shouldBe` Nothing  -- col 9 out of range for bench
      parsePosition "abc" `shouldBe` Nothing

    it "identifies position-like strings" $ do
      looksLikePosition "23" `shouldBe` True
      looksLikePosition "B5" `shouldBe` True
      looksLikePosition "b0" `shouldBe` True
      looksLikePosition "ANI" `shouldBe` False
      looksLikePosition "ani.2" `shouldBe` False
      looksLikePosition "123" `shouldBe` False

    it "formats positions correctly" $ do
      formatPosition (Position 0 0) `shouldBe` "00"
      formatPosition (Position 2 3) `shouldBe` "23"
      formatPosition (Position 4 5) `shouldBe` "B5"

  describe "allCommands" $ do
    it "includes all basic commands" $ do
      let commandNames = map fst allCommands
      commandNames `shouldContain` ["print"]
      commandNames `shouldContain` ["help"]
      commandNames `shouldContain` ["quit"]
      commandNames `shouldContain` ["add <entity>[.stars] [pos] [*]"]
      commandNames `shouldContain` ["upgrade <entity|pos>"]
      commandNames `shouldContain` ["sell <entity|pos>..."]
      commandNames `shouldContain` ["find <query>"]
