import Monad

data MyState s a = MyState { runMyState :: s -> (a,s) }

fromStoAandS :: Int -> (String, Int)
fromStoAandS c 
             | c `mod` 5 == 0 = ("foo", c+1)
             | otherwise      = ("bar", c+1)

stateIntString :: MyState Int String
stateIntString = MyState fromStoAandS


test1 :: (String, Int)
test1 = runMyState stateIntString 0 