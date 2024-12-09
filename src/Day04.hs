module Day04 where

import Paths_aoc2024 (getDataFileName)

import Data.List (transpose)
import Data.List (isPrefixOf)

type Grid = [[Char]]

countOccurrences :: Eq a => [a] -> [a] -> Int
countOccurrences _ [] = 0
countOccurrences needle (x:xs)
  | needle `isPrefixOf` (x:xs) = 1 + countOccurrences needle xs
  | otherwise = countOccurrences needle xs

diagonals :: [[a]] -> [[a]]
diagonals []       = []
diagonals ([]:xss) = xss
diagonals xss      = zipWith (++) (map (\xs -> case xs of
                                                 (y:_) -> [y]
                                                 []    -> []) xss ++ repeat [])
                                  ([]:(diagonals (map (drop 1) xss)))

-- build up a list of all possible sequences to search for (horizontal, vertical, diagonal and there reverse)
createSearchGrid ::  Grid -> Grid
createSearchGrid grid = grid
            ++ map reverse grid
            ++ transpose grid
            ++ map reverse (transpose grid)
            ++ diagonals grid
            ++ map reverse (diagonals grid)
            ++ diagonals (map reverse grid)
            ++ map reverse (diagonals (map reverse grid))


searchXMASes :: [[Char]] -> Int -> Int -> Bool
searchXMASes matrix row col
  | row < 1 || row >= length matrix - 1
    || col < 1 || col >= length (head matrix) - 1
    || (matrix !! row !! col) /= 'A' = False
  | otherwise =
      let diagonals = [ matrix !! (row - 1) !! (col - 1)
                      , matrix !! (row - 1) !! (col + 1)
                      , matrix !! (row + 1) !! (col - 1)
                      , matrix !! (row + 1) !! (col + 1)
                      ]
          diagonalStr = diagonals
      in diagonalStr `elem` ["MSMS", "MMSS", "SSMM", "SMSM"]

traverseAndCountXMASes :: [[Char]] -> Int
traverseAndCountXMASes matrix =
  foldl' (\count (row, col) -> if searchXMASes matrix row col then count + 1 else count) 0 positions
  where
    numRows = length matrix
    numCols = if null matrix then 0 else length (head matrix)
    positions = [(row, col) | row <- [0 .. numRows - 1], col <- [0 .. numCols - 1]]

day04 :: IO ()
day04 = do
  inputLines <- lines <$> (getDataFileName "day04-input.txt" >>= readFile)

  let searchGrid = createSearchGrid inputLines
--  putStrLn $ "This is the search grid:" ++ show searchGrid

  let occurrences = sum $ map (countOccurrences "XMAS") searchGrid
  putStrLn $ "Part 1: " ++ show occurrences

  let xmases = traverseAndCountXMASes inputLines
  putStrLn $ "Part 2: " ++ show xmases


