import Prelude hiding (words, unwords, lines, unlines, length, readFile, writeFile)
import System.IO hiding (hGetContents, hPutStr, readFile, writeFile)
import System.Environment
import Data.ByteString.Lazy.Char8 hiding (filter, putStrLn)

import Data.Char

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put


block0 :: ByteString
block0 = runPut $ do
	mapM (\_ -> putWord32le 0)  [1 .. 26]
	return ()


numberOfOffsets = 26
blockSize = numberOfOffsets * 4

readOffsets :: Get [Int]
readOffsets = readOffsets' numberOfOffsets
readOffsets' 0 = return []
readOffsets' n = do
	x <- getWord32le
	xs <- readOffsets' (n-1)	
	return (fromIntegral x : xs)
	

process :: Get Int
process = do
	x <- getWord32le
	return (fromIntegral x)
	


writeInitialBlock :: Handle -> IO ()
writeInitialBlock handle = do
	hPutStr handle block0
	return ()


addWord :: Handle -> String -> IO ()
addWord handle string = do
	hSeek handle SeekFromEnd 0
	pos <- hTell handle
	if pos == 0 
		then 
			writeInitialBlock handle 
		else 
			return ()
	
	hSeek handle AbsoluteSeek 0
	bs <- hGetContents handle

	let offsets = runGet readOffsets bs

	putStrLn (show offsets)

	return ()
	
	



main = do
	[file] <- getArgs
	handle <- openFile file ReadWriteMode
	addWord handle "ABC"
	--bs <- readFile file
	--let x = runGet process bs
	--let bs = runPut block0
	--writeFile file bs
	--putStrLn (show x)

	return ()

			
	
