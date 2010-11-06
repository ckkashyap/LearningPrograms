module Main where

import Network.Server
import Network.Socket
import Control.Monad
import System.IO

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.Binary.Get
import Data.Binary.Put
import Data.Word


main :: IO ()
main = do
	running <- serveOne (Just $ UserWithDefaultGroup "ckk") server
	putStrLn "server is accepting connections!!!"
	waitFor running

	where server = Server (SockAddrInet 5901 iNADDR_ANY) Stream doVNC


doVNC :: ServerRoutine
doVNC (h,n,p) = do startRFB h


startRFB :: Handle -> IO ()
startRFB h = do
		hPutStr h "RFB 003.003\n"
		hFlush h
		
		clientHeaderByteStream <- BS.hGet h 12
		putStrLn (show clientHeaderByteStream)
		let (m,n) = ( runGet readClientHeader clientHeaderByteStream)

		-- Send 1 to the client, meaning, no auth required
		BS.hPutStr h (BS.pack [0,0,0,1])
		hFlush h

		clientInitMessage <- BS.hGet h 1

		let sharedOrNot = runGet (do {x<-getWord8;return(x);}) clientInitMessage

		putStrLn (show sharedOrNot)


		BS.hPutStr h serverInitMessage
		hFlush h






serverInitMessage :: BS.ByteString
serverInitMessage = runPut $ do
				putWord16be (300::Word16) -- width
				putWord16be (300::Word16) -- height
				--pixel format
				putWord8 (32::Word8) -- bits per pixl
				putWord8 (24::Word8) -- depth
				putWord8 (1::Word8) -- big endian
				putWord8 (1::Word8) -- true color
				putWord16be (255::Word16) -- red max
				putWord16be (255::Word16) -- green max
				putWord16be (255::Word16) -- blue max
				putWord8 (24::Word8) -- red shift
				putWord8 (1::Word8)  -- green shift
				putWord8 (1::Word8)  -- blue shift
				--padding
				putWord8 (0::Word8)
				putWord8 (0::Word8)
				putWord8 (0::Word8)
				--name length
				let name = "Haskell Framebuffer"
				putWord32be (((fromIntegral.length) name)::Word32)
				--mapM_ (putWord8.fromIntegral.ord) name
				putLazyByteString (B.pack name)

				--putLazyByteString (stringToByteString name)


abcd = map (putWord8.fromIntegral.ord) "ABCD"


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

			

word8ToByteString :: Word8 -> BS.ByteString
word8ToByteString n = runPut $ putWord8 n

word16ToByteString :: Word16 -> BS.ByteString
word16ToByteString n = runPut $ putWord16be n


word32ToByteString :: Word32 -> BS.ByteString
word32ToByteString n = runPut $ putWord32be n


stringToByteString :: String -> BS.ByteString
stringToByteString str = BS.pack (map (fromIntegral.ord) str)

