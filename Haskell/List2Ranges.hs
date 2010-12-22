range n [] = (n-1,[])
range n (x:xs) = if n == x then range (n+1) xs else (n-1,x:xs)

collect [] = []
collect xs = _collect (head xs) xs

_collect n xs = part1 ++ part2
	where
		(nn,list) = range n xs
		part1 = [(n,nn)]
		part2 = collect list


list2ranges [] = []
list2ranges ((s,e):xs) = if s/=e then (show s)++".."++(show e) ++ "," ++ (list2ranges xs) else (show s)++ "," ++ (list2ranges xs)

output=list2ranges $ collect [1,2,3,6,7,8,10,12,13,14]
