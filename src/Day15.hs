module Day15 where

import Paths_aoc2024 (getDataFileName)

day15 :: IO ()
day15 = do
  inputLines <- lines <$> (getDataFileName "day15-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ unlines inputLines
  putStrLn "TODO: implement Day 15"
