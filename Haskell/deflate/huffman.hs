import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Bits
import Data.List

type CodeLength = Int
type CodeLengths = [CodeLength]
type Count = Int
type Code = Int
type CodeLengthCounts = Array.Array CodeLength Count

input :: CodeLengths
--input = [2,1,3,3]
input = [3, 3, 3, 3, 3, 2, 4, 4]

bl_count :: CodeLengths -> CodeLengthCounts
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

minCodeValForLength :: CodeLengthCounts -> CodeLength -> Code
minCodeValForLength _ 0 = 0
minCodeValForLength clc cl= (v1 + v2) `shiftL` 1
	where
		v1 = minCodeValForLength clc prevCl
		v2 = clc Array.! prevCl
		prevCl = cl - 1
	

------------------------------


codes :: CodeLengthCounts -> CodeLength -> [Code]
codes clc cl = codes' cl cnt min
	where
		min = minCodeValForLength (bl_count input) cl
		cnt = clc Array.! cl
		codes' n 0 _ = []
		codes' n cnt min = min:(codes' n (cnt-1) (min+1))
		


testCodes :: CodeLengths -> [Code]
testCodes input = concat $ map (codes clc) (nub input)
	where
		clc = bl_count input
