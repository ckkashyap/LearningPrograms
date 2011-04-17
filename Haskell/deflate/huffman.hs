import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Bits
import Data.List

type CodeLength = Int
type CodeLengths = [CodeLength]
type Count = Int

type CodeLengthCounts = Map.Map CodeLength Count

input :: CodeLengths
input = [2,1,3,3]

bl_count :: CodeLengthCounts -> CodeLengths -> CodeLengthCounts
bl_count m [] = m
bl_count m (x:xs) = bl_count newMap xs
	where
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
			



