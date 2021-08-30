{-# OPTIONS_GHC -Wall #-}


module LogAnalysis where
import Log
import Data.List.Split

parseMessage :: String -> LogMessage
parseMessage mesg = case words mesg of
                  ("I":t:s)   -> LogMessage Info    (read t :: Int)  ((unwords s) :: String)
                  ("W":t:s)   -> LogMessage Warning (read t :: Int)  ((unwords s) :: String)
                  ("E":i:t:s) -> LogMessage (Error  (read i :: Int)) (read t :: Int) ((unwords s) :: String)
                  _           -> Unknown mesg

parse :: String -> [LogMessage]
parse n
  | n == [] = []
  | otherwise = parseMessage str : parse ls
       where
	       str = unwords (take 1 $ lines n)
               ls  = unlines (drop 1 $ lines n)


-- | Define a function: 
-- insert :: LogMessage -> MessageTree -> MessageTree
-- which inserts a new LogMessage into an existing MessageTree, pro-
--ducing a new MessageTree. insert may assume that it is given a
--sorted MessageTree, and must produce a new sorted MessageTree
--containing the new LogMessage in addition to the contents of the
--original MessageTree.
--However, note that if insert is given a LogMessage which is
--Unknown, it should return the MessageTree unchanged 



insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree

insert lm Leaf = Node Leaf lm Leaf

insert lm1@(LogMessage _ ts1 _) (Node lt lm2@(LogMessage _ ts2 _ ) rt )
  | ts1 > ts2 = Node lt lm2 (insert lm1 rt)
  | otherwise = Node (insert lm2 lt) lm1 rt 

build :: [LogMessage] -> MessageTree 
build (l:ls)             = createTree (Node Leaf l Leaf) ls 
                          where 
				  createTree :: MessageTree -> [LogMessage] -> MessageTree
				  createTree tr []                 = tr
                                  createTree tr (l:ls)             = createTree (insert l tr) ls   



