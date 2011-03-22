{-# LANGUAGE NamedFieldPuns #-}

module TreeEdsl (process,TreeContext,endTree,addLeaf,addSubTree) where

import Control.Monad.State
import qualified Data.Tree as T

data TreeGenerator = TreeGenerator {treeStack :: [T.Tree String]} deriving Show

type TreeContext a = StateT TreeGenerator IO a

root = T.Node {T.rootLabel = "root", T.subForest = [] }

initTree = TreeGenerator {treeStack=[root]}


tag2tree :: String -> T.Tree String
tag2tree str = T.Node {T.rootLabel=str, T.subForest = []} 


addSubTree :: String -> TreeContext ()
addSubTree t = do
	add t

addLeaf :: String -> TreeContext ()
addLeaf t = do
	add t
	endTree


add :: String -> TreeContext ()
add t = do
	state <- get
	let oldStack = treeStack state
	let subTree = tag2tree t
	
	let newStack = subTree:oldStack
	put (TreeGenerator newStack)
	return ()

endTree :: TreeContext ()
endTree = do
	state <- get
	let stack = treeStack state
	if (length stack > 1) then
		do
			let (e1:e2:rest) = stack
			let childList = T.subForest e2
			let newE2 = e2 {T.subForest=e1:childList}
			let newStack = newE2:rest
			put (TreeGenerator newStack)
			return ()

		else
			return ()

process :: TreeContext () -> IO (T.Tree String)
process action = do
	(s,TreeGenerator x) <- runStateT (action>>endTree) initTree
	return (x!!0)

