module Main where

import Network.Server
import Network.Socket
import Control.Monad
import System.IO

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Word


main :: IO ()
main = do
	running <- serveOne (Just $ UserWithDefaultGroup "ckk") server
	putStrLn "server is accepting connections!!!"
	waitFor running

	where server = Server (SockAddrInet 5901 iNADDR_ANY) Stream doVNC



byteString2Number :: BS.ByteString -> Int
byteString2Number bs = _byteString2Number 1 (digits bs)
	where
		_byteString2Number _ [] = 0
		_byteString2Number n (x:xs) = (n*x) + (_byteString2Number (n*10) xs)
		digits bs = map ((+(-48)).fromIntegral) (BS.unpack(BS.reverse bs))


readClientHeader  = do
	getLazyByteString 4
	m <- getLazyByteString 3 
	getWord8
	n <- getLazyByteString 3
	getWord8
	let majorVersionNumber = byteString2Number m
	let minorVersionNumber = byteString2Number n
	if (majorVersionNumber /= 3) then 
		fail ("ERROR: Unsupported version " ++ (show majorVersionNumber))
		else 
		return (byteString2Number m,byteString2Number n)

startRFB :: Handle -> IO ()
startRFB h = do
		hPutStrLn h "RFB 003.003"
		hFlush h
		x <- BS.hGet h 12
		let (m,n) = ( runGet readClientHeader x)
		hPutStrLn h (show m)
		hFlush h
		return ()
			
-- |the simple echo server routine
doVNC :: ServerRoutine
doVNC (h,n,p) = do startRFB h
