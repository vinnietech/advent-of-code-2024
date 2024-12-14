module Day05 where
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.List (elemIndex, sortBy)

import Paths_aoc2024 (getDataFileName)


parseInput :: String -> ([(Int, Int)], [[Int]])
parseInput input =
  let
    -- Split the input into two parts based on an empty line
    (ruleLines, updateLines) = case splitOn [""] (lines input) of
      (r:u:_) -> (r, u)
      _       -> ([], [])

    -- Parse rules as (Int, Int) tuples
    rules = mapMaybe (\line -> case splitOn "|" line of
      [v1, v2] -> Just (read v1, read v2)
      _        -> Nothing) ruleLines

    -- Parse update lines as lists of Ints
    updates = map (map read . splitOn ",") updateLines
  in
    (rules, updates)

checkRule :: (Int, Int) -> [Int] -> Bool
checkRule (e1, e2) u = case (elemIndex e1 u, elemIndex e2 u) of
    (Nothing, _)     -> True
    (_, Nothing)     -> True
    (Just i, Just j) -> i < j


checkAllRules :: [(Int, Int)] -> [Int] -> Bool
checkAllRules rules update = all (\ rule -> checkRule rule update) rules

sortRule :: [(Int, Int)] -> Int -> Int -> Ordering
sortRule rules e1 e2
    | (e1, e2) `elem` rules = LT
    | (e2, e1) `elem` rules = GT
    | otherwise             = EQ

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

day05 :: IO ()
day05 = do
  inputLines <- lines <$> (getDataFileName "day05-input.txt" >>= readFile)

  let (rules, updates) = parseInput (unlines inputLines)
--  putStrLn $ "Rules: " ++ show rules
--  putStrLn $ "Updates: " ++ show updates


  let part1 = sum . map middle . filter (checkAllRules rules) $ updates
  putStrLn $ "Part 1: " ++ show part1

  let part2 = sum . map middle . map (sortBy (sortRule rules)) . filter (not . checkAllRules rules) $ updates
  putStrLn $ "Part 2: " ++ show part2