module Main (main) where

import Data.Either (fromRight)
import qualified Data.Map as M
import qualified Data.Time as T
import Text.Parsec
import Text.Parsec.String (Parser)

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

parseLine :: Parser (String, (String, String))
parseLine = do
  start <- many1 (letter <|> digit) <* spaces
  _ <- char '=' <* spaces
  [left, right] <- between (char '(') (char ')') ((spaces *> many1 (letter <|> digit) <* spaces) `sepBy` char ',')
  return (start, (left, right))

parseNode :: String -> (String, (String, String))
parseNode = either (error . show) id . parse parseLine ""

step :: M.Map String (String, String) -> String -> Char -> String
step m state dir = case M.lookup state m of
  Just (l, r) -> if dir == 'L' then l else r
  Nothing -> error "Invalid State"

breakingFoldl :: (a -> Bool) -> (a -> b -> a) -> a -> [b] -> a
breakingFoldl _ _ acc [] = acc
breakingFoldl cond f acc (x : xs) = if cond acc then acc else breakingFoldl cond f (f acc x) xs

run :: String -> (String -> Bool) -> String -> M.Map String (String, String) -> Int
run start endCond dirs m = fst $ breakingFoldl (endCond . snd) (step' m) (0, start) $ cycle dirs
  where
    step' m' (n, state) d = (n + 1, step m' state d)

part1 :: String -> M.Map String (String, String) -> Int
part1 = run "AAA" (== "ZZZ")

startPoints :: M.Map String (String, String) -> [String]
startPoints m = filter ((== 'A') . last) $ M.keys m

part2 :: String -> M.Map String (String, String) -> Int
part2 dirs m = foldl lcm 1 $ map run' $ startPoints m
  where
    run' s = run s ((== 'Z') . last) dirs m

main :: IO ()
main = do
  inp <- loadInput "08/input.txt"
  let directions = head inp
  let nw = tail $ tail inp
  s <- T.getCurrentTime
  let parsedNw = M.fromList <$> map parseNode $ nw
  p <- T.getCurrentTime
  let r1 = part1 directions parsedNw
  p1 <- T.getCurrentTime
  let r2 = part2 directions parsedNw

  e <- T.getCurrentTime
  putStrLn $ "Parsing Time: " ++ show (1e9 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p s)) ++ " ns"
  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e9 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ns"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e9 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ns"