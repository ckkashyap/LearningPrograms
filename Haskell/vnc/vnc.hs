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

import qualified RFBConstants as RFB

main :: IO ()
main = do
	running <- serveOne (Just $ UserWithDefaultGroup "ckk") server
	putStrLn "server is accepting connections!!!"
	waitFor running

	where server = Server (SockAddrInet 5900 iNADDR_ANY) Stream doVNC


doVNC :: ServerRoutine
doVNC (h,n,p) = do 
			startRFB h
			eventLoop h
			


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

		if sharedOrNot==1 then putStrLn "Sharing enabled"
			else putStrLn "Sharing disabled"

		BS.hPutStr h serverInitMessage
		hFlush h




eventLoop :: Handle -> IO ()
eventLoop h = do
		commandByte <- BS.hGet h 1
		let command = runGet (do {x<-getWord8;return(x);}) commandByte
		putStrLn ("Command = " ++ (show command))
		doCommand command
		eventLoop h
		where
			doCommand c
				| c==RFB.setPixelFormat = do 
					putStrLn "SetPixelFormat"
					handleSetPixelFormat h
				| c==RFB.setEncodings = do
					putStrLn "SetEncodings"
					handleSetEncoding h
				| otherwise = do
					putStrLn (show c)
					fail "DONE"
			

		
handleSetPixelFormat :: Handle -> IO ()
handleSetPixelFormat h = do
	byteString <- BS.hGet h 19
	let (
		bpp,
		depth,
		bigEndian,
		trueColor,
		redMax,
		greenMax,
		blueMax,
		redShift,
		greenShift,
		blueShift) = dingo byteString
	putStrLn ("bpp = " ++ (show bpp))
	putStrLn ("depth = " ++ (show depth))
	putStrLn ("bigEndian = " ++ (show bigEndian))
	putStrLn ("trueColor = " ++ (show trueColor))
	putStrLn ("redMax = " ++ (show redMax))
	putStrLn ("greenMax = " ++ (show greenMax))
	putStrLn ("blueMax = " ++ (show blueMax))
	putStrLn ("redShift = " ++ (show redShift))
	putStrLn ("greenShift = " ++ (show greenShift))
	putStrLn ("blueShift = " ++ (show blueShift))
	return ()
	where
		dingo bs = runGet expr bs
		expr = do
			getWord16be
			getWord8
			bpp <- getWord8
			depth <- getWord8
			bigEndian <- getWord8
			trueColor <- getWord8
			redMax <- getWord16be
                        greenMax <- getWord16be
                        blueMax <- getWord16be
                        redShift <- getWord8
                        greenShift <- getWord8
                        blueShift <- getWord8
			return (bpp,depth,bigEndian,trueColor,redMax,greenMax,blueMax,redShift,greenShift,blueShift)
		
	
handleSetEncoding :: Handle -> IO ()
handleSetEncoding h = do
	byteString <- BS.hGet h 3
	let numberOfTypes = runGet (do {getWord8;x<-getWord16be;return(x);}) byteString
	putStrLn ("Number of supported encoding types = " ++ (show numberOfTypes))
	readWords numberOfTypes
	where
		readWords 0 = putStrLn ("")
		readWords n = do
				bs <- BS.hGet h 4
				let et = runGet (do {x<-getWord32be;return(x);}) bs
				putStr ((show et) ++ " ")
				readWords (n-1)





		


serverInitMessage :: BS.ByteString
serverInitMessage = runPut $ do
				putWord16be (400::Word16) -- width
				putWord16be (400::Word16) -- height
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
				--mapM_ (putWord8.fromIntegral.ord) name --this works
				putLazyByteString (B.pack name)
				--putLazyByteString (stringToByteString name) --this works


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

