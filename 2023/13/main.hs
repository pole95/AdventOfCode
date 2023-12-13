module Main (main) where

import Control.Applicative ((<|>))
import Data.List (find, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Time as T
import GHC.Float (int2Double)

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

reflectionHamming :: Int -> [String] -> Int
reflectionHamming n = sum . map (isReflection n)
  where
    isReflection row grid =
      let (l, r) = splitAt row grid
       in length $ filter not $ zipWith (==) (reverse l) r

score :: Int -> [String] -> Int
score diff grid = fromMaybe ((fromJust row) * 100) $ col
  where
    col = findIdx grid
    row = findIdx $ transpose grid
    findIdx g = find (\n -> reflectionHamming n g == diff) [1 .. (length (head g) - 1)]

part1 :: [[String]] -> Int
part1 = sum . map (score 0)

part2 :: [[String]] -> Int
part2 = sum . map (score 1)

main :: IO ()
main = do
  grids <- splitOn [""] <$> loadInput "13/input.txt"

  p <- T.getCurrentTime
  let r1 = part1 grids
  p1 <- T.getCurrentTime
  let r2 = part2 grids
  e <- T.getCurrentTime

  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e9 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ns"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e9 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ns"