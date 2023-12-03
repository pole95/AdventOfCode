module Main (main) where

import Data.Char (isDigit)
import Data.List (nub)
import qualified Data.Map as M
import Data.Time as T

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

type Map = [String]

width :: Map -> Int
width = length . head

height :: Map -> Int
height = length

at :: Map -> (Int, Int) -> Char
at m (x, y) = m !! y !! x

isSymbol :: Char -> Bool
isSymbol c = c /= '.' && (not . isDigit) c

neighbourCoords :: Map -> (Int, Int) -> [(Int, Int)]
neighbourCoords m (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0, x + dx >= 0, x + dx < width m, y + dy >= 0, y + dy < height m]

neighbours :: Map -> (Int, Int) -> [Char]
neighbours m (x, y) = map (at m) $ neighbourCoords m (x, y)

parseLine :: Map -> Int -> [(Int, Char, (Int, Int))]
parseLine m y = map report $ filter hasSymbolNeighbour $ uncurry zip $ numPositions $ foldl readNumber (0, [], [], []) indices
  where
    w = width m
    indices = [0 .. w - 1]
    readNumber :: (Int, [Int], [Int], [[Int]]) -> Int -> (Int, [Int], [Int], [[Int]])
    readNumber (currNum, allNum, currPos, allPos) x
      | null currPos && (not . isDigit) (at m (x, y)) = (currNum, allNum, currPos, allPos)
      | (not . isDigit) (at m (x, y)) = (0, currNum : allNum, [], currPos : allPos)
      | otherwise = (currNum * 10 + read [at m (x, y)], allNum, x : currPos, allPos)
    numPositions :: (Int, [Int], [Int], [[Int]]) -> ([Int], [[Int]])
    numPositions (currNum, allNum, currPos, allPos)
      | null currPos = (allNum, allPos)
      | otherwise = (currNum : allNum, currPos : allPos)
    hasSymbolNeighbour :: (Int, [Int]) -> Bool
    hasSymbolNeighbour (_, cs) = any (\x -> any isSymbol (neighbours m (x, y))) cs
    report :: (Int, [Int]) -> (Int, Char, (Int, Int))
    report (c, cs) = (c, at m pos, pos)
      where
        bs = nub $ concatMap (\x -> neighbourCoords m (x, y)) cs
        pos = head $ filter (isSymbol . at m) bs

findGears :: [(Int, Char, (Int, Int))] -> M.Map (Int, Int) [Int]
findGears = foldl insert M.empty
  where
    insert m (c, g, pos) = M.insertWith (++) pos ([c | g == '*']) m

getRatios :: M.Map (Int, Int) [Int] -> [Int]
getRatios = map product . filter (\gs -> length gs == 2) . M.elems

parseMap :: Map -> [(Int, Char, (Int, Int))]
parseMap m = concatMap (parseLine m) [0 .. height m - 1]

part1 :: Map -> Int
part1 m = sum $ map (\(num, _, _) -> num) $ parseMap m

part2 :: Map -> Int
part2 m = sum $ getRatios $ findGears $ parseMap m

main :: IO ()
main = do
  inp <- loadInput "03/input.txt"
  p <- T.getCurrentTime
  let r1 = part1 inp
  p1 <- T.getCurrentTime
  let r2 = part2 inp
  e <- T.getCurrentTime
  putStrLn $ "Part 1: " ++ show r1 ++ " | Time: " ++ show (1000 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ms"
  putStrLn $ "Part 2: " ++ show r2 ++ " | Time: " ++ show (1000 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"