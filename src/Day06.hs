module Day06 where

import Paths_aoc2024 (getDataFileName)

import Data.Array

import Data.Maybe (listToMaybe)
import Data.List (nub)

type Matrix a = Array (Int, Int) a
type Position = (Int, Int)
type Direction = (Int, Int)


-- | Walks from a position in the grid according to the given direction.
walk :: Matrix Char -> (Position, Direction) -> [(Position, Direction)]
walk grid (currentPosition@(y, x), direction@(dy, dx)) =
  let
    -- Calculate the next position
    newY = y + dy
    newX = x + dx

    -- Check if the position is out of bounds
    ((rowMin, colMin), (rowMax, colMax)) = bounds grid
    isOutOfBounds = newY < rowMin || newY > rowMax || newX < colMin || newX > colMax

    -- Get the current character from the grid
    charAtPosition = if isOutOfBounds then Nothing else Just (grid ! (newY, newX))

    -- Check if the character at the new position is a #
    isObstacle = charAtPosition == Just '#'

    -- Produce the next step of the walk
    nextStep =
      if isOutOfBounds
      then []
      else if isObstacle
      then walk grid (currentPosition, (dx, -dy)) -- Turn right
      else walk grid ((newY, newX), direction) -- Move forward
  in
    (currentPosition, direction) : nextStep


parseGrid :: String -> Matrix Char
parseGrid input =
  let
    rows = lines input
    firstRow = case listToMaybe rows of
      Just row -> row
      Nothing -> error "Grid is empty, cannot parse"
    rowCount = length rows
    colCount = length firstRow
    indicesAndValues =
      [((row, col), char)
       | (row, line) <- zip [0..] rows
       , (col, char) <- zip [0..] line
      ]
  in
    array ((0, 0), (rowCount - 1, colCount - 1)) indicesAndValues

-- | Detects if a list contains a cycle using Floyd's Tortoise and Hare algorithm.
isLoop :: Eq a => [a] -> Bool
isLoop a = go a a
  where
    go (x:xs) (_:y:ys) = x == y || go xs ys
    go _      _        = False

day06 :: IO ()
day06 = do
  inputLines <- lines <$> (getDataFileName "day06-input.txt" >>= readFile)

  let grid = parseGrid $ unlines inputLines
--  putStrLn $ "Grid: " ++ show grid

  let guardStartPosition =
            case listToMaybe [ (y, x) | ((y, x), char) <- assocs grid, char == '^'] of
              Just position -> position -- We found a position
              Nothing       -> error "No guard (^) found in the grid"

  putStrLn $ "Guard start position: " ++ show guardStartPosition

  let visitedPositions = walk grid (guardStartPosition, (-1, 0))
--  putStrLn $ "Visited positions: " ++ show visitedPositions

  let distinctPositionVisited = length $ nub (fst <$> visitedPositions)
  putStrLn $ "Part 1: " ++ show distinctPositionVisited

  -- remove the first position (guard position) from the list of visited positions
  let candidateObstacles = drop 1 (nub (fst <$> visitedPositions))
--  putStrLn $ "Candidate obstacles: " ++ show candidateObstacles

  let possibleGrids = map (\position -> grid // [(position, '#')]) candidateObstacles
--  putStrLn $ "Possible grids: " ++ show possibleGrids

 -- Check if each grid causes the guard to loop
  let gridLoops = map (\g -> isLoop (walk g (guardStartPosition, (-1, 0)))) possibleGrids
--  putStrLn $ "Grid loops: " ++ show gridLoops

  let part2 = length $ filter id gridLoops
  putStrLn $ "Part 2: " ++ show part2
