import Prelude hiding (words, unwords, lines, unlines, length)
import System.IO hiding (hGetContents, hPutStr)
import System.Environment
import System.Directory
import Text.Regex.Posix
import Data.ByteString.Lazy.Char8 hiding (map)

modify :: ByteString -> ByteString
modify = unlines . map f . lines where
	f = unwords . map g . words
	g x = if matches (pack "XYZ") x then pack "ABC" else x
	matches pat str = 0 /= length b where
			(a,b,c,d) = str =~ pat ::(ByteString, ByteString, ByteString, [ByteString]) 

tempFileName =  (++) ".temp"

main = do
	[file] <- getArgs
	inHandle <- openFile file ReadMode
	contents <- hGetContents inHandle
	outHandle <- openFile (tempFileName file) WriteMode
	let modifiedContents = modify contents
	hPutStr outHandle modifiedContents

	mapM hClose [inHandle, outHandle]

	removeFile file
	renameFile (tempFileName file) file
