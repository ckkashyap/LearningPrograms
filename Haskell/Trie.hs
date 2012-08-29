data Bit = One | Zero | Star deriving Show

instance Eq Bit where
	Star == _ = True
	_ == Star = True
	One == One = True
	Zero == Zero = True
	_ == _ = False

data Word = Word [Bit] deriving Show

data Trie = EndNode {listOfPatterns :: [Word]} | Fork {left :: Trie, right :: Trie} | Nil deriving Show

word2Trie :: Word -> Trie
word2Trie word@(Word bits) = foldr f (EndNode [word]) bits
	where
		f b rest = case b of
			One -> Fork Nil rest
			Zero -> Fork rest Nil
			Star -> Fork rest rest

merge :: Trie -> Trie -> Trie
merge Nil new = new
merge original@(EndNode bs1) new@(EndNode bs2) = EndNode (bs1 ++ bs2)
merge original@(Fork l1 r1) new@(Fork l2 r2) = Fork (merge l1 l2) (merge r1 r2)
merge original _ = original

mergeFromList :: [Trie] -> Trie
mergeFromList = foldr merge Nil

match :: Trie -> [Bit] -> (Bool, [Word])
match Nil _ = (False, [])
match (EndNode bs) _ = (True, bs)
match (Fork l r) (One:rest) = match r rest
match (Fork l r) (Zero:rest) = match l rest

p1 = Word [Star,  Star]
p2 = Word [One, Zero]
p3 = Word [Zero, Zero]

t1 = word2Trie p1
t2 = word2Trie p2 
t3 = word2Trie p3

s1 = [One, One]
s2 = [One, Zero]
s3 = [Zero, One]


