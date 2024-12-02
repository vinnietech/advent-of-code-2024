{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Day01 where

import Data.List (sort)
import Paths_aoc2024 (getDataFileName)

-- First we need to read the input line by line and add the first digit to the left list and the second digit to the right list
-- Then sort the two lists
-- Then iterate over the two lists and calculate the distance and save this to a second list
-- Add up the distances between all of the pairs is the puzzle anwser

parseNumbers :: [String] -> ([Int], [Int])
parseNumbers inputLines = unzip $ map parseLine inputLines
  where
    parseLine line =
      case words line of
        [first, second] -> (read first, read second)
        _               -> error $ "Invalid line format: " ++ line

calculateDistances :: [Int] -> [Int] -> [Int]
calculateDistances xs ys = zipWith (\x y -> abs (x - y)) xs ys

countOccurrences :: [Int] -> [Int] -> [(Int, Int)]
countOccurrences xs ys = [(x, count x ys) | x <- xs]
  where
    count x ys = length (filter (== x) ys)

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)
--  putStrLn "This is what I read from input:"
--  putStrLn $ unlines inputLines

  let (firstNumbers, secondNumbers) = parseNumbers inputLines
--  putStrLn $ "First numbers: " ++ show firstNumbers
--  putStrLn $ "Second numbers: " ++ show secondNumbers

  let distances = calculateDistances (sort firstNumbers) (sort secondNumbers)
--  putStrLn $ "Distances: " ++ show distances

  let sumOfDistances = sum distances
  putStrLn $ "Sum of distances: " ++ show sumOfDistances

  let occurrences = countOccurrences firstNumbers secondNumbers
  putStrLn $ "Occurrences: " ++ show occurrences

  let multiplyOccurrences = map (\(x, y) -> x * y) occurrences
  putStrLn $ "Multiply occurrences: " ++ show multiplyOccurrences

  let sumOfMultiplyOccurrences = sum multiplyOccurrences
  putStrLn $ "Sum of multiply occurrences: " ++ show sumOfMultiplyOccurrences

