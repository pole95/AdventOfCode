{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Applicative ((<**>))
import Data.Either (fromRight)
import Data.Time as T
import Text.Parsec
import Text.Parsec.String (Parser)

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

data RGB = RGB Int Int Int
  deriving (Show)

instance Semigroup RGB where
  (<>) (RGB a b c) (RGB d e f) = RGB (a + d) (b + e) (c + f)

instance Monoid RGB where
  mempty = RGB 0 0 0

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseBall :: Parser RGB
parseBall =
  parseInt
    <**> choice
      [ try $ string " red" >> pure (\x -> RGB x 0 0),
        try $ string " green" >> pure (\x -> RGB 0 x 0),
        try $ string " blue" >> pure (\x -> RGB 0 0 x)
      ]

parseRGB :: Parser RGB
parseRGB = chainl parseBall (string ", " >> pure (<>)) mempty

parseLine :: Parser [RGB]
parseLine = string "Game " *> parseInt *> string ": " *> parseRGB `sepBy` string "; "

part1 :: [[RGB]] -> Int
part1 xs = sum $ zipWith (\i r -> (if all f r then i else 0)) [1 ..] xs
  where
    f (RGB r g b) = r <= 12 && g <= 13 && b <= 14

part2 :: [[RGB]] -> Int
part2 xs = sum $ map (cube . f) xs
  where
    f = foldr (\(RGB a b c) (RGB d e f) -> RGB (max a d) (max b e) (max c f)) mempty
    cube (RGB r g b) = r * g * b

main :: IO ()
main = do
  input <- loadInput "02/input.txt"
  s <- T.getCurrentTime
  let !parsed = map (fromRight (error "Parser Error") . parse parseLine "") input
  p <- T.getCurrentTime
  let !r1 = part1 parsed
  p1 <- T.getCurrentTime
  let !r2 = part2 parsed
  e <- T.getCurrentTime
  putStrLn $ "Parsing Time: " ++ show (1000 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p s)) ++ " ms"
  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1000 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ms"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1000 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"
