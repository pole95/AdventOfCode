module Main (main) where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)
import Data.Time as T

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

maybeDigit :: String -> Maybe Int
maybeDigit [] = Nothing
maybeDigit (x : _)
  | isDigit x = Just $ digitToInt x
  | otherwise = Nothing

maybeDigitP2 :: String -> Maybe Int
maybeDigitP2 [] = Nothing
maybeDigitP2 input
  | "one" `isPrefixOf` input = Just 1
  | "two" `isPrefixOf` input = Just 2
  | "three" `isPrefixOf` input = Just 3
  | "four" `isPrefixOf` input = Just 4
  | "five" `isPrefixOf` input = Just 5
  | "six" `isPrefixOf` input = Just 6
  | "seven" `isPrefixOf` input = Just 7
  | "eight" `isPrefixOf` input = Just 8
  | "nine" `isPrefixOf` input = Just 9
  | "zero" `isPrefixOf` input = Just 0
  | isDigit $ head input = Just $ digitToInt $ head input
  | otherwise = Nothing

getNumber :: (Num a) => [a] -> a
getNumber d = head d * 10 + last d

getNumbersFromString :: (String -> Maybe Int) -> String -> Int
getNumbersFromString f input = getNumber $ mapMaybe f $ tails input

part1 :: String -> Int
part1 = getNumbersFromString maybeDigit

part2 :: String -> Int
part2 = getNumbersFromString maybeDigitP2

main = do
  inp <- loadInput "01/input.txt"
  s <- T.getCurrentTime
  print $ sum $ map part1 inp
  p1 <- T.getCurrentTime
  print $ sum $ map part2 inp
  e <- T.getCurrentTime
  putStrLn $ "Part1 timing: " ++ show (1000 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 s)) ++ " ms"
  putStrLn $ "Part2 timing: " ++ show (1000 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"
