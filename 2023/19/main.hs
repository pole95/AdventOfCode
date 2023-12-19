module Main (main) where

import Data.Either (fromRight)
import Data.List.Split (splitOn)
import qualified Data.Map as M
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

main :: IO ()
main = do
  inp <- splitOn [""] <$> loadInput "19/input.txt"
  let workflows = parseWorkflows $ head inp
  let parts = parseParts $ last inp

  print $ part1 workflows parts
  putStrLn "AoC"