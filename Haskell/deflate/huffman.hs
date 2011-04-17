import qualified Data.Map as Map

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
		newMap = case val of
			Just i	-> insert (i+1)
			_	->  insert 1
		val = Map.lookup x m
		insert n = Map.insert x n m


test_bl_count = bl_count Map.empty input


