module Util (split,strip) where

split :: Eq a => [a] -> a -> [[a]]
split [] _ = []
split cs c = first:rest
	where
		(first,n) = fetchToken cs c
		rest = split (drop n cs) c

fetchToken [] _ = ([],0)
fetchToken (x:xs) c
	| c == x = ([],1)
	| otherwise = (x:word,n+1)
		where
			(word,n) = fetchToken xs c


strip = removeFromFront.removeFromEnd
	where
		removeFromFront = dropWhile (==' ')
		removeFromEnd = reverse.removeFromFront.reverse
