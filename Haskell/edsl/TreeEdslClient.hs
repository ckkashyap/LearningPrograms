import TreeEdsl
import Data.Tree


createTree :: TreeContext String ()
createTree = do
	insertSubTree "Maths" $ do
		insertLeaf "Algebra"
		insertLeaf "Geometry"
	insertSubTree "Chemistry" $ do
		insertLeaf "Physical"
		insertLeaf "Organic"
	insertSubTree "Physics" $ do
		insertLeaf "Modern"
		insertSubTree "Newtonian" $ do
			insertLeaf "Mechanics"
			insertLeaf "Optics"
	return ()
	


main = do
	tree <- process "root" createTree
	putStrLn (drawTree (fmap show tree))
	return ()
