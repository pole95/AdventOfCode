{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Array
import Data.Bits
import Data.List
import Data.Maybe (fromJust)
import qualified Data.Time as T

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

-- 0: North
-- 1: East
-- 2: South
-- 3: West
directions :: [(Int, Int)]
directions = [(0, -1), (1, 0), (0, 1), (-1, 0)]

toDirection :: Char -> (Int, Int)
toDirection c = case c of
  '|' -> (0, 2)
  '-' -> (1, 3)
  'L' -> (0, 1)
  'J' -> (0, 3)
  '7' -> (2, 3)
  'F' -> (1, 2)
  _ -> (-1, -1)

startPosition :: [String] -> (Int, Int)
startPosition g = (x, y)
  where
    y = fromJust . findIndex (\s -> 'S' `elem` s) $ g
    x = fromJust . elemIndex 'S' $ g !! y

moveCell :: Int -> (Int, Int) -> (Int, Int)
moveCell dir (x, y) = let (dx, dy) = directions !! dir in (x + dx, y + dy)

otherPipeDirection :: Char -> Int -> Int
otherPipeDirection pipe d
  | f == d = r
  | r == d = f
  | otherwise = -1
  where
    (f, r) = toDirection pipe

oppositeDirection :: Int -> Int
oppositeDirection = xor 2

getPath :: Char -> [String] -> (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
getPath startPipe grid (sx, sy) (tx, ty) dir
  | not (inRange (0, m - 1) sx) || not (inRange (0, n - 1) sy) = []
  | sx == tx && sy == ty && dir /= -1 = [(sx, sy)]
  | otherwise = if nextDir == -1 || null recurse then [] else (sx, sy) : recurse
  where
    m = length $ head grid
    n = length grid
    pipe = grid !! sy !! sx
    nextDir = if dir == -1 then fst $ toDirection startPipe else otherPipeDirection pipe (oppositeDirection dir)
    recurse = getPath startPipe grid (moveCell nextDir (sx, sy)) (tx, ty) nextDir

findLoop :: [String] -> [(Int, Int)]
findLoop grid = run correctPipe
  where
    (sx, sy) = startPosition grid
    run pipe = getPath pipe grid (sx, sy) (sx, sy) (-1)
    correctPipe = fromJust $ find (not . null . run) "|-J7F"

-- Will return 2A: https://en.wikipedia.org/wiki/Shoelace_formula
shoeLace :: [(Int, Int)] -> Int
shoeLace [_] = 0
shoeLace ((x1, y1) : (x2, y2) : xs) = (y1 + y2) * (x2 - x1) + shoeLace ((x2, y2) : xs)

part1 :: [String] -> Int
part1 grid = length (findLoop grid) `div` 2

-- https://en.wikipedia.org/wiki/Pick%27s_theorem
part2 :: [String] -> Int
part2 grid = let path = findLoop grid in (shoeLace path - (length path + 1) + 2) `div` 2

main :: IO ()
main = do
  grid <- loadInput "10/input.txt"
  p <- T.getCurrentTime
  let !r1 = part1 grid
  p1 <- T.getCurrentTime
  let !r2 = part2 grid
  e <- T.getCurrentTime

  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ms"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"

-- print $ part1 grid