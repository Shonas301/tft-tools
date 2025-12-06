module Main where

import Data.Char (toLower, isAlphaNum)
import Data.List (nub, isPrefixOf)
import qualified Data.Map as Map
import System.Environment (getArgs)

type Champion = (String, String, String, String, String, String, String)

parseCSVLine :: String -> [String]
parseCSVLine line =
    let (fields, _) = foldr processChar ([], []) line
    in reverse fields
  where
    processChar ',' (fields, currentField) = (reverse currentField : fields, [])
    processChar c (fields, currentField) = (fields, c : currentField)

sanitizeName :: String -> String
sanitizeName = filter isAlphaNum

generateShorthand :: String -> String
generateShorthand name
    | "&" `isPrefixOf` words name =
        let parts = words name
        in take 2 (sanitizeName (head parts)) ++ take 2 (sanitizeName (last parts))
    | " " `elem` name =
        let parts = words name
        in concatMap (take 2 . sanitizeName) parts
    | otherwise = take 3 (sanitizeName name)

makeUnique :: [String] -> Map.Map String Int -> [String]
makeUnique [] _ = []
makeUnique (name:names) seen =
    let shorthand = generateShorthand name
        count = Map.findWithDefault 0 shorthand seen
        uniqueShorthand = if count == 0
                         then shorthand
                         else shorthand ++ show count
        newSeen = Map.insert shorthand (count + 1) seen
    in uniqueShorthand : makeUnique names newSeen

processChampions :: String -> String
processChampions csv =
    let allLines = lines csv
        header = head allLines
        champLines = tail allLines
        champNames = map (head . parseCSVLine) champLines
        shorthands = makeUnique champNames Map.empty
        newLines = zipWith addShorthand champLines shorthands
    in unlines (header ++ ",shorthand" : newLines)
  where
    addShorthand line shorthand = line ++ "," ++ shorthand

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> do
            content <- readFile inputFile
            let processed = processChampions content
            writeFile outputFile processed
            putStrLn $ "Generated shorthands and wrote to " ++ outputFile
        _ -> putStrLn "Usage: GenerateShorthands <input.csv> <output.csv>"
