import Control.Monad.Trans
import Control.Monad.Trans.Error
import Control.Monad.Trans.State


data Data = C1 | C2 | C3 deriving Show


getData :: String -> Either String Data
getData str = case str of
			"c1" -> Right C1
			"c2" -> Right C2
			"c3" -> Right C3
			_    -> Left "Unrecognized string"


someIO :: IO (Either String Data)
someIO = do
	str <- getLine 
	let d = getData str
	return d


aa :: StateT Int (ErrorT String IO) Data
aa = do
	lift $ lift $ putStrLn "Hello World"
	modify (+1)
	--lift $ throwError "Hello World from aa"
	return C1


bb :: ErrorT String IO (Data, Int)
bb = runStateT aa 10

cc :: IO (Either String (Data, Int))
cc  = runErrorT bb






 


someIOThatMayFail :: ErrorT String IO Data
someIOThatMayFail = do
	performIOThatMayFail
	xx <- bb
	lift $ putStrLn (show xx)
	l <- lift getLine
	let d = getData l
	case d of
		Right x -> return x
		_       -> throwError "What the hell"


performIOThatMayFail :: ErrorT String IO Data
performIOThatMayFail = do
	x <- ErrorT someIO
	return x



main :: IO ()
main = do
	x <- runErrorT someIOThatMayFail
	putStrLn (show x)
	return ()
	



