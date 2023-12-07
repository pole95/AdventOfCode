module Main (main) where

import Data.List (group, sort, sortBy)
import qualified Data.Time as T

data Hand = Hand String Int deriving (Show, Eq)

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

lexicographicCards :: Char -> Char
lexicographicCards c
  | c == 'A' = 'E'
  | c == 'K' = 'D'
  | c == 'Q' = 'C'
  | c == 'J' = 'B'
  | c == 'T' = 'A'
  | otherwise = c

lexicographicCards' :: Char -> Char
lexicographicCards' c
  | c == 'A' = 'E'
  | c == 'K' = 'D'
  | c == 'Q' = 'C'
  | c == 'J' = '0'
  | c == 'T' = 'A'
  | otherwise = c

formatCards :: String -> String
formatCards = map lexicographicCards

formatCards' :: String -> String
formatCards' = map lexicographicCards'

parse :: (String -> String) -> [String] -> [Hand]
parse format = map parse'
  where
    parse' s = case words s of
      [hand, bit] -> Hand (format hand) (read bit)
      _ -> error "Invalid Input"

numJokers :: String -> Int
numJokers = length . filter (== '0')

rank :: String -> Int
rank hand = case sortBy (flip compare) $ map length $ group $ sort hand of
  [5] -> 7
  [4, _] -> 6
  [3, 2] -> 5
  [3, _, _] -> 4
  [2, 2, _] -> 3
  [2, _, _, _] -> 2
  _ -> 1

rank' :: String -> Int
rank' hand = case (numJokers hand, sortBy (flip compare) $ map length $ group $ sort hand) of
  (_, [5]) -> 7
  (4, [4, _]) -> 7
  (1, [4, _]) -> 7
  (_, [4, _]) -> 6
  (3, [3, 2]) -> 7
  (2, [3, 2]) -> 7
  (_, [3, 2]) -> 5
  (3, [3, _, _]) -> 6
  (1, [3, _, _]) -> 6
  (_, [3, _, _]) -> 4
  (2, [2, 2, _]) -> 6
  (1, [2, 2, _]) -> 5
  (_, [2, 2, _]) -> 3
  (2, [2, _, _, _]) -> 4
  (1, [2, _, _, _]) -> 4
  (_, [2, _, _, _]) -> 2
  (1, _) -> 2
  (_, _) -> 1

compareHands :: (String -> Int) -> Hand -> Hand -> Ordering
compareHands ranking (Hand h1 _) (Hand h2 _)
  | ranking h1 > ranking h2 = GT
  | ranking h2 > ranking h1 = LT
  | otherwise = compare h1 h2

-- part1 :: [Hand] -> Int
part1 :: [String] -> Int
part1 hs = foldl (\acc (i, Hand _ bid) -> acc + i * bid) 0 $ zip [1 ..] $ sortBy comparing $ parse formatCards hs
  where
    comparing = compareHands rank

part2 :: [String] -> Int
part2 hs = foldl (\acc (i, Hand _ bid) -> acc + i * bid) 0 $ zip [1 ..] $ sortBy comparing $ parse formatCards' hs
  where
    comparing = compareHands rank'

main :: IO ()
main = do
  inp <- loadInput "07/input.txt"
  p <- T.getCurrentTime

  let r1 = part1 inp
  p1 <- T.getCurrentTime

  let r2 = part2 inp
  e <- T.getCurrentTime

  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e9 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ns"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e9 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ns"