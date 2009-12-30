import Monad ( MonadPlus(..) )

data List a = Cons a (List a) | Empty
	deriving Show



myFoldR :: (a -> b -> b) -> b -> List a -> b
myFoldR f z = myFoldR_f_z
	where
		myFoldR_f_z Empty = z
		myFoldR_f_z (Cons x xs) = f x $ myFoldR_f_z xs

--myMap :: (t -> a) -> List t -> List a
myMap :: (t -> a) -> List t -> List a
myMap f = myFoldR (Cons . f) Empty


--myAppend :: List a -> List a -> List a
myAppend :: List a -> List a -> List a
myAppend xs ys = myFoldR Cons ys xs


--myConcat :: List (List a) -> List a
myConcat :: List (List a) -> List a
myConcat = myFoldR myAppend Empty

instance Monad List where
	return a = Cons a Empty
	Empty >>= f = Empty
	l >>= f = myConcat (myMap f l)


list2myList :: [a] -> List a
list2myList [] = Empty
list2myList (x:xs) = Cons x (list2myList xs)

l1 =  list2myList [1..10]
l2 = do
	x <- l1
	y <- Cons (2*x) Empty
	return y


	-- file: ch14/MultiplyTo.hs
guarded :: Bool -> List a -> List a
guarded True  l = l
guarded False _  = Empty

multiplyTo :: Int -> List (Int,Int)
multiplyTo n = do
  x <- list2myList [1..n]
  y <- list2myList [x..n]
  guarded (x * y == n) $
    return (x, y)

