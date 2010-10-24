module Main where

import qualified Data.ByteString.Lazy as BS


main :: IO ()
main = do
	c <- BS.getContents
	BS.putStr c
