module Day16 where

import Paths_aoc2024 (getDataFileName)

day16 :: IO ()
day16 = do
  inputLines <- lines <$> (getDataFileName "day16-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ unlines inputLines
  putStrLn "TODO: implement Day 16"
