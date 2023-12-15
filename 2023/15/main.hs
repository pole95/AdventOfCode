{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Char (ord)
import Data.Foldable (toList)
import Data.IntMap as M (IntMap, assocs, empty, insert, lookup)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)
import Data.Sequence as Sq (Seq ((:<|), (:|>)), filter, null, singleton, spanl, tails, (><))
import qualified Data.Time as T
import Text.Regex.TDFA ((=~))

loadInput :: String -> IO [String]
loadInput s = fmap (splitOn ",") (readFile s)

hash :: String -> Int
hash = foldl (\acc c -> (acc + ord c) * 17 `mod` 256) 0

part1 :: [String] -> Int
part1 = sum . map hash

type Box = Seq (String, Int)

type Boxes = IntMap Box

step :: Boxes -> String -> Boxes
step boxes operation
  | isNothing boxContent && op == "=" = insert box (singleton (label, focalLength)) boxes
  | isNothing boxContent && op == "-" = boxes
  | op == "=" = insert box (insertInBox label focalLength content) boxes
  | op == "-" = insert box (removeFromBox label content) boxes
  where
    label = operation =~ "[[:alpha:]]+"
    op = operation =~ "[=-]"
    focalLength = read (operation =~ "[[:digit:]]+")
    box = hash label
    boxContent = M.lookup box boxes
    (Just content) = boxContent

insertInBox :: String -> Int -> Box -> Box
insertInBox label focallLength box
  | Sq.null box = singleton (label, focallLength)
  | Sq.null . Sq.filter (\(a, b) -> a == label) $ box = box :|> (label, focallLength)
  | otherwise = (before :|> (label, focallLength)) >< after
  where
    (before, _ :<| after) = spanl (\(a, b) -> a /= label) box

removeFromBox :: String -> Box -> Box
removeFromBox label box
  | Sq.null box = box
  | Sq.null . Sq.filter (\(a, b) -> a == label) $ box = box
  | otherwise = before >< after
  where
    (before, _ :<| after) = spanl (\(a, b) -> a /= label) box

boxFocalPower :: Box -> Int
boxFocalPower b = sum $ zipWith (*) [1 ..] (toList $ fmap snd b)

part2 :: [String] -> Int
part2 = sum . map (\(a, b) -> (a + 1) * boxFocalPower b) . assocs . foldl step M.empty

main :: IO ()
main = do
  inp <- loadInput "15/input.txt"
  p <- T.getCurrentTime
  let !r1 = part1 inp
  p1 <- T.getCurrentTime
  let !r2 = part2 inp
  e <- T.getCurrentTime

  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ms"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"