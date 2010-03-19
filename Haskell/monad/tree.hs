-- Monads are Trees with Grafting

{-
class Monad m where
	return :: a -> m a
	(>>=) :: m a -> (a -> m b) -> m b
-}

data Tree a = Fork (Tree a) (Tree a) | Leaf a | Nil deriving Show


tree1 = Fork (Fork (Leaf 2) Nil) (Fork (Leaf 2) (Leaf 3))

instance Monad Tree where
	return		a = Leaf a
	Nil		>>= f = Nil
	Leaf a		>>= f = f a
	Fork u v	>>= f =Fork (u >>= f) (v >>= f)


tree2 = Fork (Leaf 2) (Leaf 3)
f 2 = Fork Nil (Leaf "Two")
f 3 = Fork Nil (Leaf "Three")

tree3 = tree2 >>= f
