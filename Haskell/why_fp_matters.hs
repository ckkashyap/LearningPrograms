

data List a = Cons a (List a) | Nil
		deriving (Show)


reduce :: (a -> b -> b) -> b -> List a -> b
reduce f x Nil = x
reduce f x (Cons a l) = f a ((reduce f x) l)

list2mylist = foldr Cons Nil


doubleall = reduce doubleandcons Nil
	where
		doubleandcons n l = Cons (2*n) l


x = list2mylist [1..10]

-- n'nary tree
data Tree a = Node a (List (Tree a)) 
		deriving (Show)

list2tree (x:[]) = Node x Nil
list2tree (x:(y:[])) = Node x (Cons (Node y Nil) Nil)
list2tree (x:xs) = Node x (Cons left (Cons right Nil))
			where
				left = list2tree (take n xs)
				right = list2tree (drop n xs)
				n = ((length xs) `div` 2)


