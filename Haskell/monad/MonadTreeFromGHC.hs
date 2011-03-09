import Control.Monad.State
data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Eq)
type Table a = [a]

numberTree :: Eq a => Tree a -> State (Table a) (Tree Int)
numberTree Nil = return Nil
numberTree (Node x t1 t2)
       =  do num <- numberNode x
             nt1 <- numberTree t1
             nt2 <- numberTree t2
             return (Node num nt1 nt2)
    where
    numberNode :: Eq a => a -> State (Table a) Int
    numberNode x
       = do table <- get
            (newTable, newPos) <- return (nNode x table)
            put newTable
            return newPos
    nNode::  (Eq a) => a -> Table a -> (Table a, Int)
    nNode x table
       = case (findIndexInList (== x) table) of
         Nothing -> (table ++ [x], length table)
         Just i  -> (table, i)
    findIndexInList :: (a -> Bool) -> [a] -> Maybe Int
    findIndexInList = findIndexInListHelp 0
    findIndexInListHelp _ _ [] = Nothing
    findIndexInListHelp count f (h:t)
       = if (f h)
         then Just count
         else findIndexInListHelp (count+1) f t


numTree :: (Eq a) => Tree a -> Tree Int
numTree t = evalState (numberTree t) []

testTree = Node "Zero" (Node "One" (Node "Two" Nil Nil) (Node "One" (Node "Zero" Nil Nil) Nil)) Nil
--numTree testTree => Node 0 (Node 1 (Node 2 Nil Nil) (Node 1 (Node 0 Nil Nil) Nil)) Nil

main = do
	putStrLn (show (numTree testTree))
