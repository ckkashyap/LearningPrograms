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
		arr = Array.array (0,max) [(i,0) | i <- [0..max]]
		max = (head.reverse.sort) l
		bl_count' [] a = a
		bl_count' (x:xs) a = bl_count' xs na
			where
				na = a Array.// [(x,v+1)]
				v = a Array.! x

------------------------------

minCode' bc 0 = 0 
minCode' bc i = ((minCode' bc (i-1) + bitCount (i-1)) `shiftL` 1)
	where
		bitCount n = bc  Array.! n
			
minCode = minCode' (bl_count input)

