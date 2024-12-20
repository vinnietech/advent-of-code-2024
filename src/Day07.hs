module Day07 where

import Data.Maybe (catMaybes)

import Paths_aoc2024 (getDataFileName)

parseInput :: String -> ([(Int, [Int])])
parseInput =
  map (\line ->
    let (k, v) = break (== ':') line
        key = read k
        values = map read (words (drop 1 v))
    in (key, values))
  . lines

calculateAllValues :: [Int] -> [Int]
calculateAllValues [] = []
calculateAllValues [x] = [x]
calculateAllValues (x:xs) =
  foldl (\acc y -> [a + y | a <- acc] ++ [a * y | a <- acc] ++ [concatInts a y | a <- acc]) [x] xs

concatInts :: Int -> Int -> Int
concatInts x y = read (show x ++ show y)

day07 :: IO ()
day07 = do
  inputLines <- lines <$> (getDataFileName "day07-input.txt" >>= readFile)

  let equations = parseInput (unlines inputLines)
  -- putStrLn $ "Equations: " ++ show equations

  let part1 = sum $ catMaybes $ map (\(target, nums) -> if target `elem` calculateAllValues nums then Just target else Nothing) equations
  putStrLn $ "Part 1: " ++ show part1