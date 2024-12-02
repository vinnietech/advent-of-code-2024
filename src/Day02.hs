module Day02 where

import Paths_aoc2024 (getDataFileName)

-- 1) Parse the input lines and create a list of reports (levels)
-- 2) Check if a report is safe by first checking if the levels are all increasing or decreasing, i.a. is monotonic
--    and second if the adjacent levels differ by at least one and at most three
--    easier: just check if the differences are all between 1 and 3 or -3 and -1
-- part 2: allow for a report to be safe by removing one level. Here I'm just brute forcing this by removing one level at a time and checking if the report is safe
--    I'm sure this can be done more efficiently, but I don't have the time today to find a more efficient solution

linesToReport :: [String] -> [[Int]]
linesToReport = map (map read . words)

isSave :: [Int] -> Bool
isSave xs =
    all (\d -> 0 < d && d <= 3) diffs || all (\d -> -3 <= d && d < 0) diffs
  where
    diffs = zipWith (-) (drop 1 xs) xs

isSaveWithDemper :: [Int] -> Bool
isSaveWithDemper xs =
    isValid xs || any isValid (removeOne xs)
  where
    diffs ys = zipWith (-) (drop 1 ys) ys
    isValid ys =
        all (\d -> 0 < d && d <= 3) (diffs ys) || all (\d -> -3 <= d && d < 0) (diffs ys)
    removeOne ys = [take i ys ++ drop (i + 1) ys | i <- [0 .. length ys - 1]]


day02 :: IO ()
day02 = do
  inputLines <- lines <$> (getDataFileName "day02-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ unlines inputLines

  let reports = linesToReport inputLines
--  putStrLn $ "List of reports: " ++ show reports

  let saveReports = filter isSave reports
--  putStrLn $ "Save reports: " ++ show saveReports

  let numberOfSaveReports = length saveReports
  putStrLn $ "Number of save reports: " ++ show numberOfSaveReports

  let saveReportsWithDemper = filter isSaveWithDemper reports
  putStrLn $ "Save reports with demper: " ++ show saveReportsWithDemper

  let numberOfSaveReportsWithDemper = length saveReportsWithDemper
  putStrLn $ "Number of save reports with demper: " ++ show numberOfSaveReportsWithDemper