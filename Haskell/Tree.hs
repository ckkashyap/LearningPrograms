data Tree a = Nil | Node a (Tree a) (Tree a)
		deriving (Show)


listToTree :: [a] -> Tree a
listToTree [] = Nil
listToTree (x:xs) = 
	Node x (listToTree half1) (listToTree half2)
	where
		half1 = take n xs
		half2 = drop n xs
		n = length xs `div` 2
