pp n o c s
	| o == n && c == n = s++" "
	| o < n = if o > c then (oo n o c s) ++ (cc n o c s) else oo n o c s
	| o > c = cc n o c s


oo n o c s = pp n (o+1) c (s++"(")
cc n o c s = pp n o (c+1) (s++")")

