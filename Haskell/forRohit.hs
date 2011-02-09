split c xs = foldr f [[]] xs
	where
		f x list@(l:ls)= if x == c then []:list else (x:l):ls


process lines = map (split ',') lines


main = do
	contents <- getContents
	let linesOfFile = lines contents
	let p = process linesOfFile
	putStrLn (show (take 2 p))

