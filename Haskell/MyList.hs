import Monad ( MonadPlus(..) )

data List a = Cons a (List a) | Empty
	deriving Show

--myMap :: (t -> a) -> List t -> List a
myMap :: (t -> a) -> List t -> List a
myMap f Empty = Empty
myMap f (Cons a rest) = Cons (f a) (myMap f rest)


--myAppend :: List a -> List a -> List a
myAppend :: List a -> List a -> List a
myAppend Empty l = l
myAppend l Empty = l
myAppend (Cons a rest) l = Cons a (myAppend rest l)


--myConcat :: List (List a) -> List a
myConcat :: List (List a) -> List a
myConcat Empty			= Empty
myConcat (Cons Empty rest)	= myConcat rest
myConcat (Cons list rest)	= myAppend list (myConcat rest)

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

