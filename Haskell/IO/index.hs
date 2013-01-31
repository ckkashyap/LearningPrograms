import Prelude hiding (words, unwords, lines, unlines, length, readFile, writeFile)
import System.IO hiding (hGetContents, hPutStr, readFile, writeFile)
import System.Environment
import Data.ByteString.Lazy.Char8 hiding (filter, putStrLn)

import Data.Char

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put


block0 :: Put
block0 = do
	mapM (\_ -> putWord32le 0)  [1 .. 26]
	return ()


read26Offsets :: Get [Int]
read26Offsets = read26Offsets' 26
read26Offsets' 0 = return []
read26Offsets' n = do
	x <- getWord32le
	xs <- read26Offsets' (n-1)	
	return (fromIntegral x : xs)
	

process :: Get Int
process = do
	x <- getWord32le
	return (fromIntegral x)
	


addWord :: Handle -> String -> IO ()
addWord handle string = do
	hSeek handle AbsoluteSeek 0

	

	

	return ()
	
	



main = do
	[file] <- getArgs
	handle <- openFile file ReadWriteMode
	addWord handle "ABC"
	--bs <- readFile file
	--let x = runGet process bs
	let bs = runPut block0
	writeFile file bs

	return ()

			
	
