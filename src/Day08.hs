module Day08 where

import Paths_aoc2024 (getDataFileName)
import Data.List (groupBy, sortOn)
import Data.Function (on)
import Data.List (tails, nub, concatMap)

type Coord = (Int, Int)
type GridSize = (Int, Int)

parseGrid :: String -> [(Coord, Char)]
parseGrid str =
  [ ((r, c), char)
  | (r, row) <- zip [0..] (lines str),
    (c, char) <- zip [0..] row,
    char /= '.'
  ]

antinodesPair :: (Coord, Coord) -> [Coord]
antinodesPair ((r1, c1), (r2, c2)) =
  [ (r1 - dr, c1 - dc)
  , (r2 + dr, c2 + dc)
  ]
  where
    dr = r2 - r1
    dc = c2 - c1

antinodesList :: [Coord] -> [Coord]
antinodesList antennas = concatMap antinodesPair [(a1, a2) | (a1:a2s) <- tails antennas, a2 <- a2s]


antinodesPair' :: Int -> (Coord, Coord) -> [Coord]
antinodesPair' maxsteps ((r1, c1), (r2, c2)) =
  [ (r1 + i * dr, c1 + i * dc)
    | i <- [(-maxsteps) .. maxsteps]
  ]
  where
    dr = r2 - r1
    dc = c2 - c1

antinodesList' :: Int -> [Coord] -> [Coord]
antinodesList' maxsteps antennas = concatMap (antinodesPair' maxsteps) [(a1, a2) | (a1:a2s) <- tails antennas, a2 <- a2s]

isWithinBounds :: Coord -> GridSize -> Bool
isWithinBounds (row, col) (height, width) =
  row >= 0 && row < height && col >= 0 && col < width

gridSize :: String -> (Int, Int)
gridSize gridStr = (height, width)
  where
    rows = lines gridStr
    height = length rows
    width = if null rows then 0 else length (head rows)

day08 :: IO ()
day08 = do
  inputLines <- lines <$> (getDataFileName "day08-input.txt" >>= readFile)

  let grid = parseGrid $ unlines inputLines
  --  putStrLn $ "Grid: " ++ show grid

  let size = gridSize (unlines inputLines)
  putStrLn $ "Grid size: " ++ show size

  let groupedAntennas = map (\group -> (snd (head group), map fst group))
                        $ (groupBy ((==) `on` snd)
                        $ (sortOn snd grid))
  -- putStrLn $ "Grouped antennas: " ++ show groupedAntennas

  let antinodesListResult = map (\(char, coords) -> (char, antinodesList coords)) groupedAntennas
  --  putStrLn $ "Antinodes list: " ++ show antinodesListResult

  let filteredAntinodesList = map (\(char, coords) -> (char, filter (`isWithinBounds` size) coords)) antinodesListResult
  -- putStrLn $ "Filtered antinodes list: " ++ show filteredAntinodesList

  let part1 = length . nub . concatMap snd $ filteredAntinodesList
  putStrLn $ "Part 1: " ++ show part1

  let antinodesListResult' = map (\(char, coords) -> (char, antinodesList' ((\(x, y) -> max x y) size) coords)) groupedAntennas
  --  putStrLn $ "Antinodes list': " ++ show antinodesListResult'

  let filteredAntinodesList' = map (\(char, coords) -> (char, filter (`isWithinBounds` size) coords)) antinodesListResult'
  --  putStrLn $ "Filtered antinodes list': " ++ show filteredAntinodesList'

  let part2 = length . nub . concatMap snd $ filteredAntinodesList'
  putStrLn $ "Part 2: " ++ show part2