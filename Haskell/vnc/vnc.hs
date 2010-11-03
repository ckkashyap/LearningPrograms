module Main where

import Network.Server
import Network.Socket
import Control.Monad
import System.IO

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Word


main2 :: IO ()
main2 = do
	running <- serveOne (Just $ UserWithDefaultGroup "ckk") server
	putStrLn "server is accepting connections!!!"
	waitFor running

	where server = Server (SockAddrInet 5901 iNADDR_ANY) Stream echo

main :: IO ()
main = main2

deserializeHeader :: Get (Word8, Word16, Word32)
deserializeHeader = do
  a <- getWord8
  b <- getWord16le
  c <- getWord32le
  return (a,b,c)




--startRFB :: GHC.IO.Handle.Types.Handle -> IO ()
startRFB h = do
		hPutStrLn h "RFB 003.003"
		hFlush h
		x <- BS.hGet h 12
		let (aa,bb,cc)=(runGet deserializeHeader x)
		putStrLn (show aa) 
		putStrLn (show bb)
		return ()
			


-- |the simple echo server routine
echo :: ServerRoutine
echo (h,n,p) = do
			startRFB h

{-
	cnt <- BS.hGetContents h
	let (a,b,c) = ( runGet deserializeHeader cnt)
	hPutStrLn h (show a)
	hFlush h

	--when ((l!!0)/='q') (echo (h,n,p))

-}
