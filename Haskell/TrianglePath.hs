
{-
	bigPath is a function that takes
	a triangle and returns a path
-}
maxPath :: [[Int]] -> Int
maxPath list = maxPath' list 0
	where
		maxPath' (x:[]) c = x!!c
		maxPath' (x:xs) c = max s1 s2
			where
			s1 = (x!!c) + maxPath' xs c
			s2 = (x!!c) + maxPath' xs (c+1)


maxPath1 :: [[Int]] -> (Int,[Int])
maxPath1 list = maxPath' list 0 0 [0] 
	where
		maxPath' (x:[]) c s l = (s+x!!c , l)
		maxPath' (x:xs) c s l = if (fst a) > (fst b) then a else b
			where
			a = maxPath' xs c (s+x!!c) (0:l)
			b = maxPath' xs (c+1) (s+x!!c) (1:l)
							   


m = maxPath1 [[1],[2,1],[1,2,3]]
n = maxPath1 [[1],[1,2],[4,1,2],[2,3,1,1]]
o = maxPath1 [[1],[2,1]]
