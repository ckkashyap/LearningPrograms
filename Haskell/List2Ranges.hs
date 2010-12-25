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

f xs = foldl ff [] xs
	where 
	  	[]  `ff` i = [(i,i)]
		((s,e):ns) `ff` i = if i == e+1 then 
					(s,i):ns 
					else
					(i,i):(s,e):ns

output1 =  f [1,2,3,6,8,9,10]

foo k [] = [(k,k)]
foo k xs@((l,h):t) = if l == k+1 then (k,h):t else (k,k):xs

output2 =  foldr foo [] [1,2,3,6,8,9,10]


f1 xs = foldr (flip ff) [] xs
	where 
	  	[]  `ff` i = [(i,i)]
		((s,e):ns) `ff` i = if i+1 == e then 
					(s,i):ns 
					else
					(i,i):(s,e):ns

output3 =  f1 [1,2,3,6,8,9,10]
