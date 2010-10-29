primes = f [2,3..]
	where
		f (p:xs) = p : f [ x | x <- xs, x `mod` p /= 0]



primes1 :: Integral a => [a]
primes1 = 2 : filter isPrime [3..]

isPrime :: Integral a => a -> Bool
isPrime n = all (\x -> mod n x /= 0) . takeWhile (\x -> x*x <= n) $ primes1
