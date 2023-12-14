{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Monad (forM_)
import qualified Data.IntMap as M
import Data.List.Split (chunksOf, splitOn)
import Data.Time as T

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

type Mapping = M.IntMap (Int, Int)

parse :: [[String]] -> ([Int], [Mapping])
parse ([seeds] : mappings) =
  (map read $ tail $ words seeds, map parseMapping mappings)
  where
    parseMapping (_ : rest) = M.fromList $ map (\r -> let [dest, src, size] = map read $ words r in (src, (dest, size))) rest
    parseMapping _ = undefined
parse _ = undefined

part1 :: ([Int], [Mapping]) -> Int
part1 (seeds, mappings) = minimum $ map (\seed -> foldl seedMap seed mappings) seeds
  where
    seedMap seed mappings = case M.lookupLE seed mappings of
      Just (src, (dest, size)) | seed < src + size -> dest + (seed - src)
      _ -> seed

part2 :: ([Int], [Mapping]) -> Int
part2 (seeds, mappings) = fst $ minimum $ foldl mapRanges ranges mappings
  where
    ranges = map (\[a, size] -> (a, a + size)) $ chunksOf 2 seeds
    mapRanges :: [(Int, Int)] -> Mapping -> [(Int, Int)]
    mapRanges ranges m = concatMap (go m) ranges
      where
        go m (a, b) = case M.lookupLE a m of
          Nothing -> let Just (kmin, _) = M.lookupMin m in if kmin > b then [(a, b)] else (a, kmin - 1) : go m (kmin, b)
          Just (src, (dest, size)) | a >= src + size -> [(a, b)]
          Just (src, (dest, size)) | b > src + size -> (dest + (a - src), dest + size) : go m (src + size, b)
          Just (src, (dest, size)) -> [(dest + (a - src), dest + (b - src))]

main :: IO ()
main = do
  inp <- loadInput "05/input.txt"
  let parsed = parse $ splitOn [""] inp
  p <- T.getCurrentTime

  let !r1 = part1 parsed
  p1 <- T.getCurrentTime

  let !r2 = part2 parsed
  e <- T.getCurrentTime
  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ms"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"