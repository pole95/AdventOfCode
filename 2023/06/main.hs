{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.List (transpose)
import qualified Data.Time as T

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

numWins :: Int -> Int -> Int
numWins t d
  | d <= 0 = 0
  | otherwise = r1 - r2 + 1
  where
    a = -1
    b = fromIntegral t :: Double
    c = fromIntegral (-d) :: Double
    disc = b ^ 2 - 4 * a * c
    dr1 = (-b - sqrt disc) / (2 * a)
    dr2 = (-b + sqrt disc) / (2 * a)
    r1 = if floor dr1 == truncate dr1 then floor dr1 - 1 else floor dr1
    r2 = if ceiling dr2 == truncate dr2 then ceiling dr2 + 1 else floor dr2

part1 :: [(Int, Int)] -> Int
part1 = product . map (uncurry numWins)

part2 :: [(Int, Int)] -> Int
part2 = uncurry numWins . foldl (\(x, y) (a, b) -> (read (show x ++ show a), read (show y ++ show b))) (0, 0)

stringToList :: String -> [Int]
stringToList s = map read $ tail $ words s

main :: IO ()
main = do
  inp <- loadInput "06/input.txt"
  let pairs = map (\[a, b] -> (a, b)) $ transpose $ map stringToList inp
  p <- T.getCurrentTime

  let !r1 = part1 pairs
  p1 <- T.getCurrentTime

  let !r2 = part2 pairs
  e <- T.getCurrentTime
  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ms"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"
