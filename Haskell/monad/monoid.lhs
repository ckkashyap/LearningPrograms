%include polycode.fmt

> import Data.Monoid
> import Data.Foldable
> import Control.Monad.Writer
> import Control.Monad.State

> fact1 :: Integer -> Writer String Integer
> fact1 0 = return 1
> fact1 n = do
>   let n' = n-1
>   tell $ "We've taken one away from " ++ show n ++ "\n"
>   m <- fact1 n'
>   tell $ "We've called f " ++ show m ++ "\n"
>   let r = n*m
>   tell $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
>   return r


> fact5 :: Integer -> Writer (Dual String) Integer
> fact5 0 = return 1
> fact5 n = do
>   let n' = n-1
>   tell $ Dual $ "We've taken one away from " ++ show n ++ "\n"
>   m <- fact5 n'
>   tell $ Dual $ "We've called f " ++ show m ++ "\n"
>   let r = n*m
>   tell $ Dual $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
>   return r

> ex5 = runWriter (fact5 10)


> tellFst a = tell $ (a,mempty)
> tellSnd b = tell $ (mempty,b)

> fact6 :: Integer -> Writer (String,Sum Integer) Integer
> fact6 0 = return 1
> fact6 n = do
>   let n' = n-1
>   tellSnd (Sum 1)
>   tellFst $ "We've taken one away from " ++ show n ++ "\n"
>   m <- fact6 n'
>   let r = n*m
>   tellSnd (Sum 1)
>   tellFst $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
>   return r

> ex6 = runWriter (fact6 5)

