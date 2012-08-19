import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe


hello :: IO (Maybe String)
hello =  do
	putStrLn "Say Something"
	l <- getLine
	if l == "HELLO" then return (Just l) else return Nothing


world :: MaybeT IO String
world = MaybeT hello

askPassword :: MaybeT  IO String
askPassword = do
	lift $ putStrLn "Enter Password:"
	l <- lift getLine
	world

	lift $ putStrLn "Enter Password:"
	l <- lift getLine
	return l

runIt = runMaybeT askPassword 

main = do
	x <- runIt
	putStrLn (show x)
