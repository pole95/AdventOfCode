{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Data.Time as T

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

parse :: String -> [Int]
parse = map read . words

diff :: (Num a) => [a] -> [a]
diff a = zipWith (-) (tail a) a

extrapolate :: [Int] -> Int
extrapolate xs = fst $ until allZero step (0, xs)
  where
    allZero (_, ys) = all (== 0) ys
    step (n, ys) = (n + last ys, diff ys)

part1 :: [[Int]] -> Int
part1 = sum . map extrapolate

part2 :: [[Int]] -> Int
part2 = sum . map (extrapolate . reverse)

main :: IO ()
main = do
  inp <- loadInput "09/input.txt"
  let parsed = map parse inp
  p <- T.getCurrentTime
  let !r1 = part1 parsed
  p1 <- T.getCurrentTime
  let !r2 = part2 parsed
  e <- T.getCurrentTime
  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ms"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"
