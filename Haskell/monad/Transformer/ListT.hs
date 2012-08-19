import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.List


askPassword :: ListT IO String
askPassword = do
	lift $ putStrLn "Enter Password:"
	l <- lift getLine
	return l
	lift $ putStrLn "Enter Password:"
	l <- lift getLine
	o <- get
	return l

runIt = runListT askPassword 

main = do
	x <- runIt
	putStrLn (show x)
