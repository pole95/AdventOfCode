{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Arrow ((&&&))
import Control.Monad.State
import Data.Either (fromRight)
import Data.List (intersect, sortBy)
import qualified Data.Map as M
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

type Block = (Int, Int, Int)

data Brick = Brick Block Block deriving (Show, Eq, Ord)

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

parseBlock :: Parser Block
parseBlock = do
  x <- read <$> P.many1 P.digit <* P.char ','
  y <- read <$> P.many1 P.digit <* P.char ','
  z <- read <$> P.many1 P.digit
  return (x, y, z)

parseBrick :: Parser Brick
parseBrick = do
  s <- parseBlock <* P.string "~"
  e <- parseBlock <* P.spaces
  return $ Brick s e

isBelow :: Brick -> Brick -> Bool
isBelow b@(Brick (_, _, z1) (_, _, z2)) b'@(Brick (_, _, z1') (_, _, z2')) = max z1 z2 == min z1' z2' - 1 && b /= b'

supports :: Brick -> Brick -> Bool
supports b b' = isBelow b b' && not (null $ intersect (squares b) (squares b'))
  where
    squares (Brick (x1, y1, _) (x2, y2, _)) = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

isBlocked :: Brick -> [Brick] -> Bool
isBlocked n m = bottom n || any (`supports` n) m
  where
    bottom (Brick (_, _, z1) (_, _, z2)) = min z1 z2 == 1

letFall :: Brick -> Brick
letFall (Brick (x1, y1, z1) (x2, y2, z2)) = Brick (x1, y1, z1 - 1) (x2, y2, z2 - 1)

updatePlane :: [Brick] -> Brick -> [Brick]
updatePlane ms m = (if isBlocked m ms then m else letFall m) : ms

step :: [Brick] -> [Brick]
step = reverse . foldl updatePlane [] . sortBricks
  where
    sortBricks :: [Brick] -> [Brick]
    sortBricks = sortBy (\(Brick (_, _, z1) (_, _, z2)) (Brick (_, _, z3) (_, _, z4)) -> compare (min z1 z2) (min z3 z4))

isDone :: [Brick] -> Bool
isDone m = m == step m

letSettle :: [Brick] -> [Brick]
letSettle = until isDone step

getSupports :: [Brick] -> (M.Map Brick [Brick], M.Map Brick [Brick])
getSupports bs = (go &&& go') bs
  where
    go :: [Brick] -> M.Map Brick [Brick]
    go [] = M.empty
    go (b : bs') = M.insert b (filter (`supports` b) bs) $ go bs'
    go' :: [Brick] -> M.Map Brick [Brick]
    go' [] = M.empty
    go' (b : bs') = M.insert b (filter (b `supports`) bs) $ go' bs'

getRedundant :: [Brick] -> Int
getRedundant bs = length $ filter (\(_, ss) -> all (\s -> length (supported M.! s) > 1) ss) $ M.toList supports
  where
    (supported, supports) = getSupports bs

part1 :: [Brick] -> Int
part1 = getRedundant

type TowerState = State (M.Map Brick [Brick], M.Map Brick [Brick])

chain :: Brick -> TowerState Int
chain b = do
  (supported, supports) <- get
  if null (supported M.! b)
    then do
      put (M.map (filter (/= b)) supported, supports)
      (+ 1) . sum <$> mapM chain (supports M.! b)
    else return 0

clear :: Brick -> TowerState ()
clear b = do
  (supported, supports) <- get
  put (M.insert b [] supported, supports)
  return ()

reaction :: [Brick] -> Int
reaction bs = sum $ map (\b -> (-1) + evalState (clear b >> chain b) (getSupports bs)) bs

part2 :: [Brick] -> Int
part2 = reaction

main :: IO ()
main = do
  inp <- loadInput "22/input.txt"
  let !bricks = letSettle $ map (fromRight (error "Workflow parser error") . P.parse parseBrick "") inp
  print $ part1 bricks
  print $ part2 bricks
  putStrLn "AoC"