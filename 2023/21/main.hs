{-# LANGUAGE BangPatterns #-}
module Main(main) where

import qualified Data.Set as S
import qualified Data.Time as T

type Position = (Int,Int)

type Grid = (S.Set Position, Int, Int)

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

getCoords :: [String] -> Char -> [Position]
getCoords input char =
  concatMap (\(y,line)-> map (\(x,_)->(x,y))$filter (\(_,c)-> c == char) $ zip [0..] line) $ zip [0..] input

parseGrid :: [String] -> (Position,Grid)
parseGrid s = (start,(grid, w, h))
  where
    w = length $ head s
    h = length s
    start = head $ getCoords s 'S'
    grid = S.fromList $ start : getCoords s '.'

validPosition :: Grid->Position->Bool
validPosition (g,w,h) (x,y) = (x `mod` w, y `mod` h) `S.member` g

neighbours :: Grid -> Position -> S.Set Position
neighbours g (x,y) = S.fromList $ filter (validPosition g) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

visit::Grid->S.Set Position -> S.Set Position
visit grid@(g,w,h)  = S.unions . S.map (neighbours grid)

part1'::Int->(Position,Grid) -> Int
part1' n = length . run
    where 
      run (start,grid) = iterate (visit grid) (S.singleton start) !! n

part1 :: (Position, Grid) -> Int
part1 = part1' 64

part2 :: (Position,Grid) -> Int
part2 (start,grid@(_,w,_)) = case map (xs!!) $ take 3 [w`div`2,w+w`div`2..] of
  [a,b,c] -> a + n' *(b-a) + n' *(n'-1) `div`2 * ((c-b)-(b-a))
  _-> error "cannot reach"
  where
    xs = map length $ iterate (visit grid) (S.singleton start)
    n' = 26501365 `div` w

main::IO()
main= do
  inp <- loadInput "21/input.txt"
  
  s <- T.getCurrentTime
  let !startAndGrid = parseGrid inp
  p <- T.getCurrentTime
  let !r1 = part1 startAndGrid
  p1 <- T.getCurrentTime
  let !r2 = part2 startAndGrid

  e <- T.getCurrentTime
  putStrLn $ "Parsing Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p s)) ++ " ms"
  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ms"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"