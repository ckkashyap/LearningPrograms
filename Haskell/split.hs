split [] = ("",0)
split (',':r) = ("",1)
split (x:xs) = (x:word,n+1)
	where
		(word,n) = split xs

splits xs = first:rest
	where
		(first,n) = split xs
		rest = splits (drop n xs)

str = "abcd,xyz,"++str
