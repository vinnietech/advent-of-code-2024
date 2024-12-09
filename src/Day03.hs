module Day03 where
import Text.Regex.TDFA ( (=~) )
import Text.Read (readMaybe)
import Paths_aoc2024 (getDataFileName)
import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn)

-- I've used a regular expression to extract the instructions from the input and turn it into a list of tuples
-- But for the second part I got stuck using a regex and went for a split on the string instead

extractInstructions :: String -> [(Int, Int)]
extractInstructions input = mapMaybe parseMatch matches
  where
    pattern :: String
    pattern = "mul\\(([0-9]+),([0-9]+)\\)"
    matches = input =~ pattern :: [[String]]

    parseMatch :: [String] -> Maybe (Int, Int)
    parseMatch [_, x, y] = (,) <$> readMaybe x <*> readMaybe y
    parseMatch _ = Nothing

sumOfProducts :: [(Int, Int)] -> Int
sumOfProducts = sum . map (\(x, y) -> x * y)

removeDisabledInstructions :: String -> String
removeDisabledInstructions input = concat $ splitOn "do()" input >>= take 1 . splitOn "don't()"


day03 :: IO ()
day03 = do
  inputLines <- lines <$> (getDataFileName "day03-input.txt" >>= readFile)

  let instructions = extractInstructions (concat inputLines)
--  putStrLn $ "Extracted instructions as tuples: " ++ show instructions

  let total = sumOfProducts instructions
  putStrLn $ "Part 1: " ++ show total

  let enabledInstructions = removeDisabledInstructions (concat inputLines)
--  putStrLn $ "Enabled instructions: " ++ show enabledInstructions

  let instructions2 = extractInstructions enabledInstructions
--  putStrLn $ "Extracted instructions as tuples part 2: " ++ show instructions2

  let totalPart2 = sumOfProducts instructions2
  putStrLn $ "Part 2: " ++ show totalPart2

