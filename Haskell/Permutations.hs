perms :: String -> [String]
perms []	= []
perms (x:[])	= [[x]]
perms (x:xs)	= concat (f [x] (perms xs))

spread :: String -> String -> [String] -- interpolate first string at various positions of second string
spread str1 str2 = _spread str1 str2 (length str2)
			where
				_spread str1 str2 0	= [str1 ++ str2]
				_spread str1 str2 n	= [(take n str2) ++ str1 ++ (drop n str2)] ++ (_spread str1 str2 (n-1))

f xs = map (spread xs)


main = do
	print (length (perms "abcdefghijk"))
