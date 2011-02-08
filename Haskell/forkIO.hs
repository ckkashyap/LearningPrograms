import Data.IORef
import Control.Concurrent
import Control.Monad


startThread ref = forever $ do
			msg <- readIORef ref
			putStrLn (show msg )
			threadDelay 1000000
			return ()


action = do
	ref <- newIORef "Hello"
	t <- forkIO $ startThread ref
	return ()
	
{-
croudwear-lm:Haskell ckk$ ghci forkIO.hs 
GHCi, version 6.12.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Loading package ffi-1.0 ... linking ... done.
[1 of 1] Compiling Main             ( forkIO.hs, interpreted )
Ok, modules loaded: Main.
*Main> ref <- newIORef "Hello"
*Main> t<-forkIO (startThread ref)
"Hello"
*Main> "Hello"
-}


