{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Algorithm.Search (dijkstra)
import qualified Data.Bifunctor
import Data.Char (digitToInt)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Time as T

type Position = (Int, Int)

data Direction = North | East | South | West | Any deriving (Show, Eq, Ord)

type Grid = (M.Map Position Int, Int, Int)

type State = (Position, Direction, Int)

directions = [(0, -1), (1, 0), (0, 1), (-1, 0)]

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

parseGrid :: [String] -> Grid
parseGrid s = (grid, w, h)
  where
    w = length $ head s
    h = length s
    grid = M.fromList $ concat $ zipWith (\y cells -> zipWith (\x cell -> ((x, y), digitToInt cell)) [0 ..] cells) [0 ..] s

cost :: Grid -> State -> State -> Int
cost (g, _, _) _ ((x, y), _, _) = fromJust $ M.lookup (x, y) g

neighbours1 :: Grid -> State -> [State]
neighbours1 (_, w, h) ((x, y), d, n) =
  filter
    (isValid d)
    [ ((x, y + 1), South, count South d n),
      ((x, y - 1), North, count North d n),
      ((x + 1, y), East, count East d n),
      ((x - 1, y), West, count West d n)
    ]
  where
    isValid nd ((x, y), d, n) = x >= 0 && x < w && y >= 0 && y < h && n <= 3 && not (inverse nd d)
    inverse North South = True
    inverse East West = True
    inverse West East = True
    inverse South North = True
    inverse _ _ = False
    count nd d n = if nd == d then n + 1 else 1

neighbours2 :: Grid -> State -> [State]
neighbours2 (_, w, h) ((0, 0), _, 0) = [((1, 0), East, 1), ((0, 1), South, 1)]
neighbours2 (_, w, h) ((x, y), d, n)
  | n < 4 = filter (isValid d) [((x + dx d, y + dy d), d, n + 1)]
  | otherwise =
      filter
        (isValid d)
        [ ((x, y + 1), South, count South d n),
          ((x, y - 1), North, count North d n),
          ((x + 1, y), East, count East d n),
          ((x - 1, y), West, count West d n)
        ]
  where
    isValid nd ((x, y), d, n) = x >= 0 && x < w && y >= 0 && y < h && n <= 10 && not (inverse nd d)
    inverse North South = True
    inverse East West = True
    inverse West East = True
    inverse South North = True
    inverse _ _ = False
    count nd d n = if nd == d then n + 1 else 1
    dx West = -1
    dx East = 1
    dx _ = 0
    dy North = -1
    dy South = 1
    dy _ = 0

solved :: Grid -> State -> Bool
solved (_, w, h) ((x, y), _, _) = (x, y) == (w - 1, h - 1)

part1 :: Grid -> Int
part1 grid@(g, w, h) = fst $ fromJust $ dijkstra (neighbours1 grid) (cost grid) (solved grid) ((0, 0), Any, 0)

part2 :: Grid -> Int
part2 grid@(g, w, h) = fst $ fromJust $ dijkstra (neighbours2 grid) (cost grid) (solved grid) ((0, 0), Any, 0)

main :: IO ()
main = do
  inp <- parseGrid <$> loadInput "17/input.txt"
  p <- T.getCurrentTime
  let !r1 = part1 inp
  p1 <- T.getCurrentTime
  let !r2 = part2 inp
  e <- T.getCurrentTime

  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ms"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"