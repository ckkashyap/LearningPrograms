

primeNumber = 23

base        = 5

pvtKeyAlice = 6
pvtKeyBob   = 15


modCompute :: Integer -> Integer -> Integer  -> Integer
modCompute a b c = (round ((fromIntegral a) ** (fromIntegral b))) `mod` c

alice2bob = modCompute base pvtKeyAlice primeNumber 

bob2alice = modCompute base pvtKeyBob primeNumber  


secretWithAlice = modCompute bob2alice pvtKeyAlice primeNumber

secretWithBob = modCompute alice2bob pvtKeyBob primeNumber