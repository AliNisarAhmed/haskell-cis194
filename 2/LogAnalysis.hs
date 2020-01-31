{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import           Data.List (sortOn)
import           Log

parseMessage :: String -> LogMessage
parseMessage str = checkMessage $ words str
  where
    checkMessage ("I":x:ys) = LogMessage Info (read x :: Int) (unwords ys)
    checkMessage ("E":s:x:ys) = LogMessage (Error (read s :: Int)) (read x :: Int) (unwords ys)
    checkMessage ("W":x:ys) = LogMessage Warning (read x :: Int) (unwords ys)
    checkMessage invalid = Unknown $ unwords invalid

parse :: String -> [LogMessage]
parse = fmap parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ ts _) node@(Node left msg2@(LogMessage _ t _) right) =
  case compare ts t of
    EQ -> node
    LT -> Node (insert msg1 left) msg2 right
    GT -> Node left msg2 (insert msg1 right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                  = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ t _) = t
getTimeStamp (Unknown _ )       = 0

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = foldr step [] . sortOn getTimeStamp
  where
    step (LogMessage (Error s) _ str) acc
      | s > 50 = str:acc
      | otherwise = acc
    step _ acc = acc
