module Day09 where

import Paths_aoc2024 (getDataFileName)

import Data.Char (digitToInt)
import Data.List (sortOn)
import Data.List (delete, insert)


data Block = File Int | Free deriving (Eq)

instance Show Block where
  show (File n) = show n
  show Free     = "."

parseInput :: String -> ([Block])
parseInput input = concatMap expandChar (zip [0..] input)
  where
    expandChar :: (Int, Char) -> [Block]
    expandChar (idx, ch)
        | odd idx   = replicate n Free
        | even idx  = replicate n (File (idx `div` 2) )
        | otherwise = []
      where
        n = digitToInt ch


defragment :: [Block] -> [Block]
defragment xs = truncatedResult ++ replicate freeCount Free
  where
    (result, freeCount) = go xs (filter isFile (reverse xs)) 0
    truncatedResult = take (length xs - freeCount) result

    go [] _ count = ([], count)
    go _ [] count = ([], count)
    go (f : fs) (r : rs) count
        | isFile f  = let (res, c) = go fs (r : rs) count
                      in (f : res, c)
        | isFree f  = let (res, c) = go fs rs (count + 1)
                      in (r : res, c)
        | otherwise = ([], count)

    isFile :: Block -> Bool
    isFile (File _) = True
    isFile Free     = False

    isFree :: Block -> Bool
    isFree Free     = True
    isFree (File _) = False

calculateCheckSum :: [Block] -> Integer
calculateCheckSum = sum . map toInteger . zipWith (*) [0..] . map blockValue
  where
    blockValue :: Block -> Int
    blockValue (File n) = n
    blockValue Free     = 0

--
--    The second part required a completely different data structure, so needed to rewrite the whole thing
--


type Lenght = Int
type Number = Int
type Index = Int

data FileBlock = FileBlock Index Lenght Number deriving (Show, Eq)
data FreeBlock = FreeBlock Index Lenght deriving (Show, Eq)

instance Ord FreeBlock where
    compare (FreeBlock idx1 _) (FreeBlock idx2 _) = compare idx1 idx2

parseInputPart2 :: String -> ([FileBlock], [FreeBlock])
parseInputPart2 input =
    let (fileList, freeList) = fst (foldl go (([], []), 0) (zip [0..] input))
    in (reverse fileList, reverse freeList)
  where
    go ((fileList, freeList), accIdx) (idx, ch) =
      let n = digitToInt ch
      in if odd idx
         then ((fileList, FreeBlock accIdx n : freeList), accIdx + n)
         else ((FileBlock accIdx n (idx `div` 2) : fileList, freeList), accIdx + n)


move :: [FreeBlock] -> FileBlock -> FreeBlock
move freeBlocks (FileBlock pos len _) = if null candidates
                                            then FreeBlock pos len
                                            else head candidates
    where
        candidates = filter (\(FreeBlock p l) -> len <= l && p < pos) freeBlocks

defragmentPart2 :: [FileBlock] -> [FreeBlock] -> [FileBlock]
defragmentPart2 [] _ = []
defragmentPart2 blocks freeBlocks = go (reverse blocks) freeBlocks
  where
    go [] _ = []
    go (FileBlock idx len num : rest) freeBlocks =
        if diff == 0
        then FileBlock p len num : go rest (delete goal freeBlocks)
        else FileBlock p len num : go rest (insert (FreeBlock (p + len) diff) (delete goal freeBlocks))
      where
        goal@(FreeBlock p l) = move freeBlocks (FileBlock idx len num)
        diff                 = l - len


sortFileBlocks :: [FileBlock] -> [FileBlock]
sortFileBlocks = sortOn getIndex
  where
    getIndex (FileBlock idx _ _) = idx


calculateCheckSumPart2 :: [FileBlock] -> Integer
calculateCheckSumPart2 blocks = sum . map toInteger . zipWith (*) [0..] $ flattenedValues
  where
    flattenedValues = concatMap expandBlock blocks

    expandBlock :: FileBlock -> [Int]
    expandBlock (FileBlock _ len num) = replicate len num


normalizeFileBlocks :: [FileBlock] -> [FileBlock]
normalizeFileBlocks [] = []
normalizeFileBlocks (b@(FileBlock idx len _) : rest) = b : fillGap (idx + len) rest
  where
    fillGap _ [] = []
    fillGap expectedIndex (next@(FileBlock nextIdx nextLen _) : xs)
      | expectedIndex == nextIdx = next : fillGap (nextIdx + nextLen) xs
      | otherwise = FileBlock expectedIndex (nextIdx - expectedIndex) 0 : fillGap nextIdx (next : xs)


day09 :: IO ()
day09 = do
  inputLines <- (getDataFileName "day09-input.txt" >>= readFile)

  let diskBlocks = parseInput inputLines
  --  putStrLn $ "Disk blocks: " ++ concatMap show diskBlocks

  let defragmented = defragment diskBlocks
  --  putStrLn $ "Defragmented: " ++ concatMap show defragmented

  let checkSum = calculateCheckSum defragmented
  putStrLn $ "Part 1: " ++ show checkSum


  let (fileBlocks, freeBlocks) = parseInputPart2 inputLines

  let defragmented' = sortFileBlocks $ defragmentPart2 fileBlocks freeBlocks

  let checkSum' = calculateCheckSumPart2 $ normalizeFileBlocks defragmented'
  putStrLn $ "Part 2: " ++ show checkSum'
