module Main where

import qualified Data.ByteString as BS


main :: IO ()
main = do
 contents <- BS.getContents
 BS.putStr contents
