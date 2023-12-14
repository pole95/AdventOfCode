{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Either (fromRight)
import Data.Functor.Classes (Show1 (liftShowsPrec))
import Data.Time as T
import Text.Parsec
import Text.Parsec.String (Parser)

type NumberList = ([Int], [Int])

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseList :: Parser [[Int]]
parseList = many (parseInt <* spaces) `sepBy` (string "|" <* spaces)

parseLine :: Parser [[Int]]
parseLine = string "Card" *> spaces *> parseInt *> string ":" *> spaces *> parseList

intersect :: [Int] -> [Int] -> [Int]
intersect [] = const []
intersect xs = filter (`elem` xs)

numWins :: [[Int]] -> Int
numWins ls = length (head ls `intersect` last ls)

part1 :: [[Int]] -> Int
part1 ls = if l > 0 then 2 ^ (l - 1) else 0
  where
    l = numWins ls

part2 :: [[[Int]]] -> Int
part2 ls = fst $ foldl go (0, [1, 1 ..]) wins
  where
    wins = map numWins ls
    go (totalCards, currCopy : restCopies) currentWins =
      let newTotal = totalCards + currCopy
          additionalCopies = replicate currentWins currCopy ++ [0, 0 ..]
          newCopies = zipWith (+) additionalCopies restCopies
       in (newTotal, newCopies)

main :: IO ()
main = do
  inp <- loadInput "04/input.txt"
  s <- T.getCurrentTime

  let !parsed = map (fromRight (error "Parser Error") . parse parseLine "") inp
  p <- T.getCurrentTime

  let !r1 = sum $ map part1 parsed
  p1 <- T.getCurrentTime

  let !r2 = part2 parsed
  e <- T.getCurrentTime

  putStrLn $ "Parsing Time: " ++ show (1e9 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p s)) ++ " ms"
  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ms"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"