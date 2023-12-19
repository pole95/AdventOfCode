{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Either (fromRight)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Time as T
import Text.Parsec as P
import Text.Parsec.String (Parser)

data Category = X | M | A | S

data Condition = ConditionLT Category Int | ConditionGT Category Int

data Destination = Accepted | Rejected | Name String deriving (Eq, Ord)

data Rule = Rule Condition Destination

data Workflow = Workflow [Rule] Destination

type NamedWorkflow = (Destination, Workflow)

data Part = Part Int Int Int Int

loadInput :: String -> IO [String]
loadInput s = fmap lines (readFile s)

parseCategory :: Parser Category
parseCategory = P.choice [P.char 'x' >> return X, P.char 'm' >> return M, P.char 'a' >> return A, P.char 's' >> return S]

parseCondition :: Parser Condition
parseCondition = do
  c <- parseCategory
  P.choice
    [ P.char '<' >> ConditionLT c <$> (read <$> P.many1 P.digit),
      P.char '>' >> ConditionGT c <$> (read <$> P.many1 P.digit)
    ]

parseDestination :: Parser Destination
parseDestination =
  P.choice
    [ P.string "A" >> return Accepted,
      P.string "R" >> return Rejected,
      Name <$> P.many1 P.letter
    ]

parseRule :: Parser Rule
parseRule = do
  c <- parseCondition <* P.char ':'
  Rule c <$> parseDestination

parseWorkflow :: Parser NamedWorkflow
parseWorkflow = do
  name <- P.many1 P.letter
  (rules, defDest) <- P.between (P.char '{') (P.char '}') ((,) <$> P.sepEndBy (P.try parseRule) (P.char ',') <*> parseDestination)
  return (Name name, Workflow rules defDest)

parseRating :: Parser Int
parseRating = do
  _ <- P.anyChar <* P.char '='
  read <$> P.many1 P.digit

parsePart :: Parser Part
parsePart = do
  ratings <- P.between (P.char '{') (P.char '}') (P.sepBy parseRating (P.char ','))
  case ratings of
    [x, m, a, s] -> return $ Part x m a s
    _ -> error "Invalid Part!"

category :: Category -> Part -> Int
category X (Part x _ _ _) = x
category M (Part _ m _ _) = m
category A (Part _ _ a _) = a
category S (Part _ _ _ s) = s

condition :: Condition -> Part -> Bool
condition (ConditionLT c n) p = category c p < n
condition (ConditionGT c n) p = category c p > n

rules :: [Rule] -> Part -> Destination -> Destination
rules [] _ def = def
rules (Rule c d : rs) p def = if condition c p then d else rules rs p def

workflow :: M.Map Destination Workflow -> Part -> Destination -> Destination
workflow w p n = case M.lookup n w of
  Just (Workflow r d) -> rules r p d
  Nothing -> error "No such workflow!"

runPart :: M.Map Destination Workflow -> Part -> Int
runPart w p@(Part x m a s) = case until (\n -> n == Accepted || n == Rejected) (workflow w p) (Name "in") of
  Accepted -> x + m + a + s
  Rejected -> 0

parseWorkflows :: [String] -> M.Map Destination Workflow
parseWorkflows = M.fromList . map (fromRight (error "Workflow parser error") . P.parse parseWorkflow "")

parseParts :: [String] -> [Part]
parseParts = map (fromRight (error "Workflow parser error") . P.parse parsePart "")

part1 :: M.Map Destination Workflow -> [Part] -> Int
part1 w p = sum $ map (runPart w) p

type Range = (Int, Int)

data PartRange = PartRange Range Range Range Range deriving (Eq)

getConditionRange :: Condition -> PartRange -> (Maybe PartRange, Maybe PartRange)
getConditionRange (ConditionLT X x) (PartRange (x1, x2) m a s)
  | x <= x1 = (Nothing, Just $ PartRange (x1, x2) m a s)
  | x > x2 = (Just $ PartRange (x1, x2) m a s, Nothing)
  | otherwise = (Just $ PartRange (x1, x - 1) m a s, Just $ PartRange (x, x2) m a s)
getConditionRange (ConditionLT M m) (PartRange x (m1, m2) a s)
  | m <= m1 = (Nothing, Just $ PartRange x (m1, m2) a s)
  | m > m2 = (Just $ PartRange x (m1, m2) a s, Nothing)
  | otherwise = (Just $ PartRange x (m1, m - 1) a s, Just $ PartRange x (m, m2) a s)
getConditionRange (ConditionLT A a) (PartRange x m (a1, a2) s)
  | a <= a1 = (Nothing, Just $ PartRange x m (a1, a2) s)
  | a > a2 = (Just $ PartRange x m (a1, a2) s, Nothing)
  | otherwise = (Just $ PartRange x m (a1, a - 1) s, Just $ PartRange x m (a, a2) s)
getConditionRange (ConditionLT S s) (PartRange x m a (s1, s2))
  | s <= s1 = (Nothing, Just $ PartRange x m a (s1, s2))
  | s > s2 = (Just $ PartRange x m a (s1, s2), Nothing)
  | otherwise = (Just $ PartRange x m a (s1, s - 1), Just $ PartRange x m a (s, s2))
getConditionRange (ConditionGT X x) (PartRange (x1, x2) m a s)
  | x < x1 = (Just $ PartRange (x1, x2) m a s, Nothing)
  | x >= x2 = (Nothing, Just $ PartRange (x1, x2) m a s)
  | otherwise = (Just $ PartRange (x + 1, x2) m a s, Just $ PartRange (x1, x) m a s)
getConditionRange (ConditionGT M m) (PartRange x (m1, m2) a s)
  | m < m1 = (Just $ PartRange x (m1, m2) a s, Nothing)
  | m >= m2 = (Nothing, Just $ PartRange x (m1, m2) a s)
  | otherwise = (Just $ PartRange x (m + 1, m2) a s, Just $ PartRange x (m1, m) a s)
getConditionRange (ConditionGT A a) (PartRange x m (a1, a2) s)
  | a < a1 = (Just $ PartRange x m (a1, a2) s, Nothing)
  | a >= a2 = (Nothing, Just $ PartRange x m (a1, a2) s)
  | otherwise = (Just $ PartRange x m (a + 1, a2) s, Just $ PartRange x m (a1, a) s)
getConditionRange (ConditionGT S s) (PartRange x m a (s1, s2))
  | s < s1 = (Just $ PartRange x m a (s1, s2), Nothing)
  | s >= s2 = (Nothing, Just $ PartRange x m a (s1, s2))
  | otherwise = (Just $ PartRange x m a (s + 1, s2), Just $ PartRange x m a (s1, s))

getRuleRanges :: [Rule] -> Maybe PartRange -> Destination -> [(PartRange, Destination)]
getRuleRanges _ Nothing _ = []
getRuleRanges [] (Just r) def = [(r, def)]
getRuleRanges (Rule c n : rs) (Just r) def = case getConditionRange c r of
  (Nothing, Just r') -> getRuleRanges rs (Just r') def
  (Just r', Nothing) -> [(r', n)]
  (Just r1, Just r2) -> (r1, n) : getRuleRanges rs (Just r2) def
  _ -> error "lolno"

getWorkflowRange :: M.Map Destination Workflow -> PartRange -> Destination -> [(PartRange, Destination)]
getWorkflowRange _ r Accepted = [(r, Accepted)]
getWorkflowRange _ r Rejected = [(r, Rejected)]
getWorkflowRange ws pr n = case M.lookup n ws of
  Just (Workflow rs def) -> getRuleRanges rs (Just pr) def
  Nothing -> error "No Workflow found"

runRanges :: M.Map Destination Workflow -> PartRange -> [(PartRange, Destination)]
runRanges ws pr = filter (\(_, d) -> d == Accepted) $ until (all (\(_, d) -> d == Accepted || d == Rejected)) (concatMap (uncurry (getWorkflowRange ws))) [(pr, Name "in")]

part2 :: M.Map Destination Workflow -> Int
part2 ws =
  sum $
    map ((\(PartRange (x1, x2) (m1, m2) (a1, a2) (s1, s2)) -> (x2 - x1 + 1) * (m2 - m1 + 1) * (a2 - a1 + 1) * (s2 - s1 + 1)) . fst) $
      runRanges ws $
        PartRange (1, 4000) (1, 4000) (1, 4000) (1, 4000)

main :: IO ()
main = do
  inp <- splitOn [""] <$> loadInput "19/input.txt"

  s <- T.getCurrentTime
  let !workflows = parseWorkflows $ head inp
  let !parts = parseParts $ last inp
  p <- T.getCurrentTime
  let !r1 = part1 workflows parts
  p1 <- T.getCurrentTime
  let !r2 = part2 workflows

  e <- T.getCurrentTime
  putStrLn $ "Parsing Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p s)) ++ " ms"
  putStrLn $ "Part 1: " ++ show r1 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime p1 p)) ++ " ms"
  putStrLn $ "Part 2: " ++ show r2 ++ "; Time: " ++ show (1e3 * realToFrac (T.nominalDiffTimeToSeconds $ T.diffUTCTime e p1)) ++ " ms"