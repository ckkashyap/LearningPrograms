module TreeEdsl (process,TreeContext,insertLeaf,insertSubTree) where

import Control.Monad.State
import Data.Tree

data TreeGenerator a = TreeGenerator {treeStack :: [Tree a]} deriving Show

type TreeContext a b = StateT (TreeGenerator a) IO b

initTree :: a -> TreeGenerator a
initTree str = TreeGenerator {treeStack=[(Node {rootLabel=str,subForest=[]})]}


tag2tree :: a -> Tree a
tag2tree str = Node {rootLabel=str, subForest = []} 

insertSubTree :: a -> TreeContext a () -> TreeContext a () 
insertSubTree str action = do
	insertNode str
	action
	endTree

insertLeaf :: a -> TreeContext a ()
insertLeaf t = do
	insertNode t
	endTree


insertNode :: a -> TreeContext a ()
insertNode t = do
	state <- get
	let oldStack = treeStack state
	let subTree = tag2tree t
	
	let newStack = subTree:oldStack
	put (TreeGenerator newStack)
	return ()

endTree :: TreeContext a ()
endTree = do
	state <- get
	let stack = treeStack state
	if (length stack > 1) then
		do
			let (e1:e2:rest) = stack
			let childList = subForest e2
			let newE2 = e2 {subForest=e1:childList}
			let newStack = newE2:rest
			put (TreeGenerator newStack)
			return ()

		else
			return ()

process :: a -> TreeContext a () -> IO (Tree a)
process str action = do
	(s,TreeGenerator x) <- runStateT (action>>endTree) (initTree str)
	return (x!!0)

