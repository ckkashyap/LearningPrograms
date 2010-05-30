countOccurance :: Char -> String -> Int
countOccurance _ []	= 0
countOccurance t (c:cs)
	| c == t	= 1 + countOccurance t cs
	| otherwise	= countOccurance t cs



algo :: Int -> [String] -> [String]
algo _ [] = []
algo n ([]:ss) = algo n ss
algo n (s:ss)
	| no == n && nc ==n = [s] ++ algo n ss
	| otherwise = algo n (ss ++ [openNode] ++ [closeNode])
		where
			no = countOccurance '(' s
			nc = countOccurance ')' s
			openNode
				| no < n = s ++ "("
				| otherwise = []
			closeNode
				| nc < no = s ++ ")"
				| otherwise = []
