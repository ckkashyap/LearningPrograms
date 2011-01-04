import Control.Monad.Writer 

bs :: (Ord a) => a -> [a] -> Bool
bs s [] = False
bs s (x:[]) = x == s
bs s xs = if s == m then True else 
		if s > m then bs s secondHalf else bs s firstHalf
		where 
			m = xs!!(medianIndex-1)
			medianIndex = (length xs) `div` 2
			firstHalf = take (medianIndex-1) xs
			secondHalf = drop medianIndex xs
			



mylength :: [a] -> Writer String Int
mylength [] = do
	tell "Reached End\n"
	return 0
mylength (x:xs) = do
	tell "Adding one\n"
	n <- mylength xs
	return (n+1)


hello = runWriter (mylength [1..10])


mbs :: (Ord a) => a -> [a] -> Writer String Bool
mbs s [] = do
	tell "Reached end\n"
	return False
mbs s (x:[]) = do
	tell $"Single element " ++ (show (x==s))
	return (x == s)
mbs s xs = do
	if s == m then do {
			tell "MATCHED\n"
			; return True }
		else do { 
			tell $ "MATCHING\n";
			if s > m then do {tell $ "GREATER\n";mbs s secondHalf }
				else do { tell $ "LESS\n";mbs s firstHalf}
		}
		where
			m = xs!!(medianIndex-1)
			medianIndex = (length xs) `div` 2
			firstHalf = take (medianIndex-1) xs
			secondHalf = drop medianIndex xs


--		if s > m then mbs s secondHalf else bs s firstHalf
--		where 

xx s xs = putStrLn ((show n) ++ "\n" ++ str)
	where
		(n,str) = runWriter (mbs s xs)
