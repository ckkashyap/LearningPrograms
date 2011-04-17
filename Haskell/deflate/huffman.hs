import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Bits
import Data.List

type CodeLength = Int
type CodeLengths = [CodeLength]
type Count = Int

input :: CodeLengths
input = [2,1,3,3]

bl_count :: CodeLengths -> Array.Array CodeLength Count
bl_count l = bl_count' l arr
	where
<<<<<<< HEAD
		arr = Array.array (0,max) [(i,0) | i <- [0..max]]
		max = (head.reverse.sort) l
		bl_count' [] a = a
		bl_count' (x:xs) a = bl_count' xs na
			where
				na = a Array.// [(x,v+1)]
				v = a Array.! x
		



=======
		newMap	= case val of
			Just i	-> insert (i+1)
			_	-> insert 1
		val	= Map.lookup x m
		insert n= Map.insert x n m


test_bl_count = bl_count (Map.insert 0 0 Map.empty) input

maxBits :: Ord a => [a] -> a
maxBits = head.reverse.sort

----------------------------

code 0 = 0 
code i = ((code (i-1) + bitCount (i-1)) `shiftL` 1)
	where
		bitCount n = let v = Map.lookup n test_bl_count in
				case v of
					Just i	-> i
					_	-> 0
			

>>>>>>> a505d0afe90a30dfeceb6f648dda67031118d7c0

------------------------------

minCode' bc 0 = 0 
minCode' bc i = ((minCode' bc (i-1) + bitCount (i-1)) `shiftL` 1)
	where
		bitCount n = bc  Array.! n
			
minCode = minCode' (bl_count input)

