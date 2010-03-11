> import Control.Monad.State
> import Control.Monad.Identity

Firstly here are two examples of the use of the State Monad. This
code isn't intended as a tutorial on the State Monad so I won't
explain how they work.

> test1 = do
>             a <- get
>             modify (+1)
>             b <- get
>             return (a,b)

> test2 = do
>             a <- get
>             modify (++"1")
>             b <- get
>             return (a,b)

> go1 = evalState test1 0
> go2 = evalState test2 "0" 

> test3 = do
>     modify (+ 1)
>     lift $ modify (++ "1")
>     a <- get
>     b <- lift get
>     return (a,b)

> go3 = runIdentity $ evalStateT (evalStateT test3 0) "0"


> test5 = do
>     modify (+ 1)
>     a <- get
>     lift (print a)
>     modify (+ 1)
>     b <- get
>     lift (print b)

> go5 = evalStateT test5 0

