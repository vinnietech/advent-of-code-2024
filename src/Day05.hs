module Day05 where

import Paths_aoc2024 (getDataFileName)

day05 :: IO ()
day05 = do
  inputLines <- lines <$> (getDataFileName "day05-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ unlines inputLines
  putStrLn "TODO: implement Day 05"
