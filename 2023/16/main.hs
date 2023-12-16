{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Time as T

type Position = (Int, Int)

data Cell = Empty | MirrorF | MirrorB | SplitterH | SplitterV deriving (Show, Eq)

data Direction = North | East | South | West deriving (Show, Eq, Ord)

type Grid = (M.Map Position Cell, Int, Int)

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

parseGrid :: [String] -> Grid
parseGrid s = (grid, w, h)
  where
    w = length $ head s
    h = length s
    grid = M.fromList $ concat $ zipWith (\y cells -> zipWith (\x cell -> ((x, y), parseCell cell)) [0 ..] cells) [0 ..] s

parseCell :: Char -> Cell
parseCell '.' = Empty
parseCell '/' = MirrorF
parseCell '\\' = MirrorB
parseCell '-' = SplitterH
parseCell '|' = SplitterV
parseCell _ = error "Invalid Cell"

emptyCell :: Position -> Direction -> (Position, Direction)
emptyCell (x, y) North = ((x, y - 1), North)
emptyCell (x, y) East = ((x + 1, y), East)
emptyCell (x, y) South = ((x, y + 1), South)
emptyCell (x, y) West = ((x - 1, y), West)

mirrorF :: Position -> Direction -> (Position, Direction)
mirrorF (x, y) North = ((x + 1, y), East)
mirrorF (x, y) East = ((x, y - 1), North)
mirrorF (x, y) South = ((x - 1, y), West)
mirrorF (x, y) West = ((x, y + 1), South)

mirrorB :: Position -> Direction -> (Position, Direction)
mirrorB (x, y) North = ((x - 1, y), West)
mirrorB (x, y) East = ((x, y + 1), South)
mirrorB (x, y) South = ((x + 1, y), East)
mirrorB (x, y) West = ((x, y - 1), North)

splitterH :: Position -> Direction -> [(Position, Direction)]
splitterH (x, y) North = [((x - 1, y), West), ((x + 1, y), East)]
splitterH (x, y) East = [((x + 1, y), East)]
splitterH (x, y) South = [((x - 1, y), West), ((x + 1, y), East)]
splitterH (x, y) West = [((x - 1, y), West)]

splitterV :: Position -> Direction -> [(Position, Direction)]
splitterV (x, y) North = [((x, y - 1), North)]
splitterV (x, y) East = [((x, y - 1), North), ((x, y + 1), South)]
splitterV (x, y) South = [((x, y + 1), South)]
splitterV (x, y) West = [((x, y - 1), North), ((x, y + 1), South)]

step :: Grid -> (Position, Direction) -> [(Position, Direction)]
step (grid, w, h) (p, dir) = filter (valid . fst) $ case grid M.! p of
  Empty -> [emptyCell p dir]
  MirrorF -> [mirrorF p dir]
  MirrorB -> [mirrorB p dir]
  SplitterH -> splitterH p dir
  SplitterV -> splitterV p dir
  where
    valid (x, y) = x >= 0 && x < w && y >= 0 && y < h

part1 :: Grid -> Int
part1 grid = S.size . S.map fst $ dfs [((0, 0), East)] (step grid) S.empty

part2 :: Grid -> Int
part2 g@(grid, w, h) = maximum energized
  where
    energized = map (\pos -> S.size . S.map fst $ dfs [pos] (step g) S.empty) positions
    positions =
      [((x, 0), South) | x <- [0 .. w - 1]]
        ++ [((x, h - 1), North) | x <- [0 .. w - 1]]
        ++ [((0, y), East) | y <- [0 .. h - 1]]
        ++ [((w - 1, y), West) | y <- [0 .. h - 1]]

dfs :: [(Position, Direction)] -> ((Position, Direction) -> [(Position, Direction)]) -> S.Set (Position, Direction) -> S.Set (Position, Direction)
dfs [] _ seen = seen
dfs (node : ns) step seen
  | S.member node seen = dfs ns step seen
  | otherwise = dfs (step node ++ ns) step (S.insert node seen)

main :: IO ()
main = do
  inp <- parseGrid <$> loadInput "16/input.txt"
  p <- T.getCurrentTime
  let !r1 = part1 inp
  p1 <- T.getCurrentTime
  let !r2 = part2 inp
  e <- T.getCurrentTime

  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ms"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"
