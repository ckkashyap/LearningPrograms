primes = f [2..]
	where
		f (p:xs) = p : f [ x | x <- xs, x `mod` p /= 0]
