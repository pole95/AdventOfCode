module Main (main) where

import qualified Data.Array as A
import Data.List (intercalate, tails)
import Data.List.NonEmpty (unfold)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Time as T

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

parse :: String -> (String, [Int])
parse l = let [input, pattern] = words l in (input, map read $ splitOn "," pattern)

solve :: (String, [Int]) -> Int
solve (inp, pattern) = get (inp, pattern)
  where
    li = length inp
    lp = length pattern
    vals = A.array ((0, 0), (li, lp)) [((x, y), isValid (drop x inp, drop y pattern)) | x <- [0 .. li], y <- [0 .. lp]]
    get (i, p) = vals A.! (li - length i, lp - length p)

    isValid :: (String, [Int]) -> Int
    isValid ([], []) = 1
    isValid ([], _) = 0
    isValid ('#' : _, []) = 0
    isValid ('.' : xs, ys) = get (xs, ys)
    isValid (xs@('#' : _), y : ys)
      | length xs >= y
          && '.' `notElem` take y xs
          && (length xs == y || xs !! y /= '#') =
          get (drop (y + 1) xs, ys)
      | otherwise = 0
    isValid ('?' : xs, ys) = get (xs, ys) + isValid ('#' : xs, ys)

part1 :: [String] -> Int
part1 = sum . map (solve . parse)

part2 :: [String] -> Int
part2 = sum . map (solve . unfold . parse)
  where
    unfold (i, p) = (intercalate "?" $ replicate 5 i, concat $ replicate 5 p)

main :: IO ()
main = do
  inp <- loadInput "12/input.txt"
  p <- T.getCurrentTime
  let r1 = part1 inp
  p1 <- T.getCurrentTime
  let r2 = part2 inp
  e <- T.getCurrentTime

  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e9 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ns"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e9 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ns"