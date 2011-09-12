{-# LANGUAGE FlexibleInstances  #-}

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


instance Monad (MyState t) where
         return a = MyState $ \s -> (a, s)
         m >>= k = MyState $ \s -> let
           (a, s') = runMyState m s
           in runMyState (k a) s'


push :: a -> MyState [a] ()
push a = MyState $ \as -> (() , a:as)

pop :: MyState [a] a
pop = MyState $ \(x:xs) -> (x,xs)


stateFunction = do
              push 10
              push 20
              x <- pop
              let y = x+1
              push y
              push 30

test3 = runMyState stateFunction []



-----------------------------------------------------------
---------- Just State -------------------------------------
-----------------------------------------------------------

data JustState a b = JustState { runJustState :: a -> b }




instance Monad (JustState Int) where
         return a = JustState (\i -> a)
--         (>>=) :: JustState a -> (a -> JustState a) -> JustState a
--         ma >>= atomb = JustState $ \a -> a
           

