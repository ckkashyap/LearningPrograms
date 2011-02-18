import System.Time
import Control.Parallel

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


mkList :: Int -> [Int]
mkList n = [1..n-1]

relPrime :: Int -> Int -> Bool
relPrime x y = gcd x y == 1

euler :: Int -> Int
euler n = length (filter (relPrime n) (mkList n))

sumEuler :: Int -> Int
sumEuler = sum . (map euler) . mkList

sumFibEuler :: Int -> Int -> Int
sumFibEuler a b = fib a + sumEuler b


parSumFibEuler :: Int -> Int -> Int
parSumFibEuler a b
	= f `par` (e + f)
		where
			f = fib a
			e = sumEuler b

secDiff :: ClockTime -> ClockTime -> Float
secDiff (TOD secs1 psecs1) (TOD secs2 psecs2)
 = fromInteger (psecs2 - psecs1) / 1e12 + fromInteger (secs2 - secs1)

r1 :: Int
r1 = parSumFibEuler 38 5300

main :: IO ()
main = do
	t0 <- getClockTime
	pseq r1 (return())
	t1 <- getClockTime
	putStrLn ("Sum: " ++ show r1)
	putStrLn ("time: " ++ show (secDiff t0 t1) ++ " seconds")

