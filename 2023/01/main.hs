module Main where


import Data.Char (isDigit, digitToInt)
import Data.Maybe (mapMaybe)
import Data.List (isPrefixOf, tails)

loadInput s =  fmap lines (readFile s)


maybeDigit :: Char -> Maybe Int
maybeDigit x
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



getNumber :: Num a => [a] -> a
getNumber d = head d * 10 + last d

part1 :: String -> Int
part1 input = getNumber $ mapMaybe maybeDigit input

part2 :: String -> Int
part2 input = getNumber $ mapMaybe maybeDigitP2 $ tails input


main = do 
    inp <- loadInput "input.txt"
    print $ sum $ map part1 inp
    print $ sum $ map part2 inp
