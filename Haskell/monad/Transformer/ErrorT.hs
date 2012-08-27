import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Error


hello :: IO (Either String Int)
hello =  do
	putStrLn "Say Something"
	l <- getLine
	if l == "HELLO" then return (Right 100) else return (Left "BOOM")


world :: ErrorT String IO Int
world = ErrorT hello


askPassword :: ErrorT String IO Int
askPassword = do
	lift $ putStrLn "Enter Password:"
	l <- lift getLine

	x <- ErrorT hello

	lift $ putStrLn "Enter Password:"
	l <- lift getLine
	return 10

runIt = runErrorT askPassword 

main = do
	x <- runIt
	putStrLn (show x)
