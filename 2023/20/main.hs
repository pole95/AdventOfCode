{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad.State (State, evalState, gets, modify)
import Data.Either (fromRight)
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

type Name = String

data Module
  = Broadcaster [Name]
  | FlipFlop Name [Name]
  | Conjunction Name [Name]
  deriving (Show, Eq)

name :: Module -> Name
name (Broadcaster _) = "broadcaster"
name (FlipFlop n _) = n
name (Conjunction n _) = n

destinations :: Module -> [Name]
destinations (Broadcaster ns) = ns
destinations (FlipFlop _ ns) = ns
destinations (Conjunction _ ns) = ns

parents :: Name -> [Module] -> [Module]
parents n = filter (elem n . destinations)

data Signal = Low | High deriving (Show, Eq)

data Status = Off | On deriving (Show, Eq)

data ModuleState
  = BroadcasterState
  | FlipFlopState Status
  | ConjunctionState (M.Map Name Signal)
  deriving (Show, Eq)

parseDestinations :: Parser [Name]
parseDestinations = P.sepBy1 (P.many1 P.letter) (P.string "," <* P.spaces)

parseBroadcaster :: Parser Module
parseBroadcaster = do
  _ <- P.string "broadcaster" <* P.spaces <* P.string "->" <* P.spaces
  Broadcaster <$> parseDestinations

parseFlipFlop :: Parser Module
parseFlipFlop = do
  _ <- P.char '%'
  n <- P.many1 P.letter <* P.spaces <* P.string "->" <* P.spaces
  FlipFlop n <$> parseDestinations

parseConjunction :: Parser Module
parseConjunction = do
  _ <- P.char '&'
  n <- P.many1 P.letter <* P.spaces <* P.string "->" <* P.spaces
  Conjunction n <$> parseDestinations

parseModule :: Parser Module
parseModule = parseBroadcaster P.<|> parseFlipFlop P.<|> parseConjunction

parseModules :: Parser [Module]
parseModules = P.many1 (parseModule <* P.spaces) <* P.eof

initialState :: [Module] -> M.Map Name (Module, ModuleState)
initialState modules = M.fromList $ map (\m -> (name m, (m, go m))) modules
  where
    go Broadcaster {} = BroadcasterState
    go (FlipFlop _ _) = FlipFlopState Off
    go (Conjunction n _) = ConjunctionState (M.fromList $ map ((,Low) . name) $ parents n modules)

type MachineState = State (M.Map Name (Module, ModuleState))

processModule :: (Name, Name, Signal) -> MachineState [(Name, Name, Signal)]
processModule (n, n', s) = do
  gets (M.lookup n) >>= \case
    Nothing -> return []
    Just (Broadcaster ns, BroadcasterState) -> return $ map (,n,s) ns
    Just (FlipFlop _ ns, FlipFlopState st) -> do
      case (s, st) of
        (High, _) -> return []
        (Low, Off) -> do
          modify $ M.insert n (FlipFlop n ns, FlipFlopState On)
          return $ map (,n,High) ns
        (Low, On) -> do
          modify $ M.insert n (FlipFlop n ns, FlipFlopState Off)
          return $ map (,n,Low) ns
    Just (Conjunction _ ns, ConjunctionState m) -> do
      let m' = M.insert n' s m
      modify $ M.insert n (Conjunction n ns, ConjunctionState m')
      return $ map (,n,if all (== High) $ M.elems m' then Low else High) ns
    _ -> return []

processModules :: [(Name, Name, Signal)] -> MachineState [(Name, Name, Signal)]
processModules xs = concat <$> mapM processModule xs

pressButton :: MachineState (Int, Int)
pressButton = go [("broadcaster", "button", Low)]
  where
    go [] = return (0, 0)
    go xs = do
      (l, h) <- processModules xs >>= go
      let l' = length $ filter (\(_, _, s) -> s == Low) xs
      let h' = length $ filter (\(_, _, s) -> s == High) xs
      return (l + l', h + h')

pressButtons :: Int -> MachineState Int
pressButtons n = go n 0 0
  where
    go 0 l h = return $ l * h
    go i l h = do
      (l', h') <- pressButton
      go (i - 1) (l + l') (h + h')

run :: Int -> [Module] -> Int
run n ms = evalState (pressButtons n) (initialState ms)

part1 :: [Module] -> Int
part1 = run 1000

main :: IO ()
main = do
  inp <- loadInput "20/input.txt"
  let !modules = map (fromRight (error "parseError") . P.parse parseModule "") inp
  print $ part1 modules