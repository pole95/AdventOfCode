{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.List (elemIndices, findIndices, intercalate, transpose)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Time as T

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

count :: (Eq a) => a -> [a] -> Int
count el = length . filter (== el)

slideLeft :: String -> String
slideLeft ls = intercalate "#" $ map slide sections
  where
    sections = splitOn "#" ls
    slide sect =
      let numRocks = count 'O' sect
       in replicate numRocks 'O' ++ replicate (length sect - numRocks) '.'

slideWest :: [String] -> [String]
slideWest = map slideLeft

slideEast :: [String] -> [String]
slideEast = map ((reverse . slideLeft) . reverse)

slideNorth :: [String] -> [String]
slideNorth = transpose . slideWest . transpose

slideSouth :: [String] -> [String]
slideSouth = transpose . slideEast . transpose

totalRockLoad :: [String] -> Int
totalRockLoad = sum . zipWith (\r l -> r * count 'O' l) [1 ..] . reverse

spinCycle :: [String] -> [String]
spinCycle = slideEast . slideSouth . slideWest . slideNorth

findFirstDup :: (Ord a) => [a] -> Maybe a
findFirstDup = findFirstDup' S.empty
  where
    findFirstDup' _ [] = Nothing
    findFirstDup' s (x : xs)
      | S.member x s = Just x
      | otherwise = findFirstDup' (S.insert x s) xs

findCycle :: (Ord a) => [a] -> (Int, Int)
findCycle ls = case findFirstDup ls of
  Nothing -> error "No Cycle in Input!"
  Just l -> (a, m)
    where
      (a : b : _) = elemIndices l ls
      m = b - a

quickSpin :: Int -> [[String]] -> [String]
quickSpin n ls = drop a ls !! mod (n - a) m
  where
    (a, m) = findCycle ls

part1 :: [String] -> Int
part1 = totalRockLoad . slideNorth

part2 :: [String] -> Int
part2 = totalRockLoad . quickSpin 1000000000 . iterate spinCycle

main :: IO ()
main = do
  grid <- loadInput "14/input.txt"
  p <- T.getCurrentTime
  let !r1 = part1 grid
  p1 <- T.getCurrentTime
  let !r2 = part2 grid
  e <- T.getCurrentTime

  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ms"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"