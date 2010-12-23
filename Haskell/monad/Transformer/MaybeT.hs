import Control.Monad
import Control.Monad.Trans
import Data.Char


newtype (Monad m) => MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }


instance Monad m => Monad (MaybeT m) where
	return  = MaybeT . return . Just
	x >>= f = MaybeT $ do 
		maybe_value <- runMaybeT x
		case maybe_value of
		    Nothing    -> return Nothing
		    Just value -> runMaybeT $ f value

instance Monad m => MonadPlus (MaybeT m) where
    mzero     = MaybeT $ return Nothing
    mplus x y = MaybeT $ do maybe_value <- runMaybeT x
                            case maybe_value of
                                 Nothing    -> runMaybeT y
                                 Just value -> runMaybeT x



instance MonadTrans MaybeT where
    lift = MaybeT . (liftM Just)


getValidPassword :: MaybeT IO String
getValidPassword = do s <- lift getLine
                      guard (isValid s)
                      return s
 
askPassword :: MaybeT IO ()
askPassword = do lift $ putStrLn "Insert your new password:"
                 value <- getValidPassword
                 lift $ putStrLn "Storing in database..."

getPassword :: MaybeT IO String
getPassword = do lift $ putStrLn "Insert your new password:"
                 value <- getValidPassword
                 lift $ putStrLn "Storing in database..."
		 return value


isValid :: String -> Bool
isValid s = length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s


f :: IO (Maybe ())
f = runMaybeT askPassword
