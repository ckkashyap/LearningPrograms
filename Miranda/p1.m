zipWith f [] b = []
zipWith f a [] = []
zipWith f (a:as) (b:bs) = (f a b):zipWith f as bs

tail [] = []
tail (a:as) = as


fib = 1:2: zipWith (+) fib (tail fib)
