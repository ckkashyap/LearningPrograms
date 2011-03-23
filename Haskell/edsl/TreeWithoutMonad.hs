{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Tree
import GHC.Exts( IsString(..) )

data TheTree a = TheTree (Tree a) deriving (Eq,Show)


instance IsString (TheTree String) where
	fromString str = TheTree ( Node {rootLabel=str, subForest=[]} )


f :: TheTree String -> String
f (TheTree t) = (show t)

(+>) :: TheTree String -> TheTree String -> TheTree String
(+>) (TheTree (Node l_root l_forest)) (TheTree (Node r_root r_forest)) = TheTree (Node l_root newForest)
	where
		newForest = l_forest ++ [Node r_root r_forest]


numberTree :: TheTree String
numberTree = "numbers" +> "1" +> "2" +> "3"

alphabetTree :: TheTree String
alphabetTree = "letters" +> "A" +> "B" +> "C"

getTree :: TheTree String
getTree = "root" +> numberTree +> alphabetTree
		

main = do
	let (TheTree x) = getTree
	putStrLn (drawTree (fmap show x))
	return ()

