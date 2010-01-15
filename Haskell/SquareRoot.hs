root n precision = guessRoot n precision 1

guessRoot n precision g
	| abs ((g*g) - n) <= precision = g
	| otherwise = guessRoot n precision ((g + (n/g))/2)


mysum [] = 0
mysum (x:xs) = x + mysum (xs)
