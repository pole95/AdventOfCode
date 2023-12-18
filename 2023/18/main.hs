{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Either (fromRight)
import qualified Data.Time as T
import Numeric (readHex)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

data Direction = U | D | L | R deriving (Show, Eq)

type Trench = (Direction, Int)

type ColorTrench = (Trench, String)

type Position = (Int, Int)

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

parseDir :: Parser Direction
parseDir =
  P.choice
    [ P.char 'U' >> return U,
      P.char 'D' >> return D,
      P.char 'L' >> return L,
      P.char 'R' >> return R
    ]

parseTrench :: Parser ColorTrench
parseTrench = do
  d <- parseDir <* P.spaces
  l <- read <$> P.many1 P.digit <* P.spaces
  c <- P.between (P.char '(') (P.char ')') (P.char '#' *> P.many1 P.hexDigit) <* P.spaces
  return ((d, l), c)

parseColor :: String -> Trench
parseColor c =
  let (l', d') = (init c, last c)
      d = case d' of
        '0' -> R
        '1' -> D
        '2' -> L
        '3' -> U
        _ -> error "Unknown Direction"
      l = case readHex l' of
        [(l, "")] -> l
        _ -> error "Invalid Length"
   in (d, l)

step :: Position -> Trench -> Position
step (x, y) (U, l) = (x, y - l)
step (x, y) (D, l) = (x, y + l)
step (x, y) (L, l) = (x - l, y)
step (x, y) (R, l) = (x + l, y)

area :: [Trench] -> Int
area = (1 +) . (`div` 2) . snd . foldl shoelace ((0, 0), 0)
  where
    shoelace ((x, y), a) p =
      let (x', y') = step (x, y) p
       in ((x', y'), a + x * y' - x' * y + abs (x - x') + abs (y - y'))

part1 :: [ColorTrench] -> Int
part1 = area . map fst

part2 :: [ColorTrench] -> Int
part2 = area . map (parseColor . snd)

main :: IO ()
main = do
  inp <- loadInput "18/input.txt"
  s <- T.getCurrentTime
  let !parsed = map (fromRight (error "Parse error") . P.parse parseTrench "") inp
  p <- T.getCurrentTime
  let !r1 = part1 parsed
  p1 <- T.getCurrentTime
  let !r2 = part2 parsed

  e <- T.getCurrentTime
  putStrLn $ "Parsing Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p s)) ++ " ms"
  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ms"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"