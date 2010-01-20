

mergeSort (x:[]) = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
	where
		left = take n xs
		right= drop n xs
		n = (length xs) `div` 2



merge xs [] = xs
merge [] xs = xs
merge xx@(x:xs) yy@(y:ys) = if x < y 
				then x:merge xs yy 
				else y:merge xx ys
