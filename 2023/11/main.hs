{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.List
import qualified Data.Time as T

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

parse :: [String] -> ([Int], [Int], [(Int, Int)])
parse xs =
  let width = length $ head xs
      height = length xs
      emptyRows = map fst $ filter (all (== '.') . snd) $ zip [0 ..] xs
      emptyColumns = map fst $ filter (all (== '.') . snd) $ zip [0 ..] $ transpose xs
      galaxies = map fst $ filter ((== '#') . snd) $ zip [(x, y) | x <- [0 .. height - 1], y <- [0 .. width - 1]] $ concat (transpose xs)
   in (emptyRows, emptyColumns, galaxies)

findGalaxyDistances :: Int -> [Int] -> [Int] -> [(Int, Int)] -> [Int]
findGalaxyDistances expansion emptyRows emptyColumns galaxies = map (sum . dists) $ (init . tails) galaxies
  where
    dists :: [(Int, Int)] -> [Int]
    dists (x : xs) = map (expandedManhattan x) xs
    dists [] = []
    lengthBetween x y ls = length (takeWhile (< max x y) $ dropWhile (< min x y) ls)
    expandedManhattan (x, y) (x', y') = abs (x - x') + expansion * lengthBetween x x' emptyColumns + abs (y - y') + expansion * lengthBetween y y' emptyRows

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

part1 :: [String] -> Int
part1 = sum . uncurry3 (findGalaxyDistances 1) . parse

part2 :: [String] -> Int
part2 = sum . uncurry3 (findGalaxyDistances (1000000 - 1)) . parse

main :: IO ()
main = do
  img <- loadInput "11/input.txt"
  p <- T.getCurrentTime
  let !r1 = part1 img
  p1 <- T.getCurrentTime
  let !r2 = part2 img
  e <- T.getCurrentTime

  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ms"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"