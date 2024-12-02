module Day13 where

import Paths_aoc2024 (getDataFileName)

day13 :: IO ()
day13 = do
  inputLines <- lines <$> (getDataFileName "day13-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ unlines inputLines
  putStrLn "TODO: implement Day 13"
