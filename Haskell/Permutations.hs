import Data.List

perms :: String -> [String]
perms []	= []
perms (x:[])	= [[x]]
perms (x:xs)	= concat (f [x] (perms xs))

spread :: String -> String -> [String] -- interpolate first string at various positions of second string
spread str1 str2 = _spread str1 str2 (length str2)
			where
				_spread str1 str2 0	= [str1 ++ str2]
				_spread str1 str2 n	= [(take n str2) ++ str1 ++ (drop n str2)] ++ (_spread str1 str2 (n-1))

f xs = map (spread xs)


main = do
	print (length (perms "abcdefghijk"))



-- After comments from the forum

perms1 [] = [[]]
perms1 (x:xs) = do
    prm <- perms1 xs
    spread [x] prm

spread' :: a -> [a] -> [[a]]
spread' x xs = zipWith (\a b -> a ++ x:b) (inits xs) (tails xs)



distinctPerms :: Ord a => [a] -> [[a]]
distinctPerms = foldr inserts [[]] . group . sort
inserts :: [a] -> [[a]] -> [[a]]
inserts xs yss = yss >>= (mingle xs)
mingle :: [a] -> [a] -> [[a]]
mingle xs [] = [xs]
mingle [] ys = [ys]
mingle xxs@(x:xs) yys@(y:ys) 
        = [x:zs | zs <- mingle xs yys] ++ [y:zs | zs <- mingle xxs ys]
