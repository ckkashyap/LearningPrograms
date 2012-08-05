import Test.QuickCheck

data Color = Red | Green | Blue deriving Show

instance Arbitrary Color where
  arbitrary = oneof [return Red, return Green, return Blue]



f :: Color -> String
f Red = "Danger"
f _ = "Safe"

prop c = collect c $ f c == "Danger"

{-
The following example shows how the reverse function could be tested.
We simply define the properties and call quickCheck on the property -

quickCheck :: Testable prop => prop -> IO ()


-}
  
prop1 x = reverse [x] == [x]

prop2 :: [Int] -> [Int] -> Property
prop2 xs ys = collect (length xs) $  reverse (xs ++ ys) == (reverse ys) ++ (reverse xs)

prop3 :: [Int] -> Bool
prop3 xs = reverse ( reverse xs ) == xs


{-

sample :: Show a => Gen a -> IO ()
sample is an interesting function that prints out all the values

*Main> sample $ choose (10,20)
16
17
10
10
14
17
15
14
16
10
19


-}


           
main = putStrLn "Hello World"
