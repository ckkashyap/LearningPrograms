{-# LANGUAGE NamedFieldPuns #-}

module XmlEdsl (process,XMLContext,getTag,add,closeTag,addLeaf,addContainer) where

import Control.Monad.State
import qualified Data.Tree as T

data Attribute = Attribute (String,String) deriving (Show)

data Tag = Tag String [Attribute] deriving (Show)

type XML = T.Tree Tag

data XMLGenerator = XMLGenerator {treeStack :: [XML]} deriving Show

type XMLContext a = StateT XMLGenerator IO a

initXML = XMLGenerator {treeStack=[root]}

root = T.Node {T.rootLabel = Tag "xml" [], T.subForest = [] }


tag2tree :: Tag -> T.Tree Tag
tag2tree t = T.Node {T.rootLabel=t, T.subForest = []} 


addContainer :: Tag -> XMLContext ()
addContainer t = do
	add t

addLeaf :: Tag -> XMLContext ()
addLeaf t = do
	add t
	closeTag


add :: Tag -> XMLContext ()
add t = do
	state <- get
	let oldStack = treeStack state
	let subTree = tag2tree t
	
	--let (parent:rest) = oldStack
	--let updatedParentChildList = subTree : (T.subForest parent) -- add the new node to the child list
	--let updatedParent = parent {T.subForest=updatedParentChildList}

	let newStack = subTree:oldStack
	put (XMLGenerator newStack)
	return ()

closeTag :: XMLContext ()
closeTag = do
	state <- get
	let stack = treeStack state
	if (length stack > 1) then
		do
			let (e1:e2:rest) = stack
			let childList = T.subForest e2
			let newE2 = e2 {T.subForest=e1:childList}
			let newStack = newE2:rest
			put (XMLGenerator newStack)
			return ()

		else
			return ()


getTag :: String -> Tag
getTag str = Tag str []

doXML :: XMLContext ()
doXML = do
	add (getTag "hello")
	add (getTag "child Of Hello")
	closeTag
	closeTag
	add (getTag "world")
	add (getTag "child Of World")
	closeTag
	closeTag
	return ()




process :: XMLContext () -> IO (XML)
process action = do
	(s,XMLGenerator x) <- runStateT (action>>closeTag) initXML
	return (x!!0)

main = do
	(s, XMLGenerator x) <- runStateT doXML initXML
	let e = x!!0
	putStrLn $ "Length = " ++ (show $ length x)
	
	--putStrLn (show x)
	putStrLn (T.drawTree (fmap show e))
	--putStrLn (T.drawTree (fmap show e2))
	return ()
