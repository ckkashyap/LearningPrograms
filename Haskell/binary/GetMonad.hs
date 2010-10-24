module Main where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Word

deserializeHeader :: Get (Word8, Word16, Word32)
deserializeHeader = do
  a <- getWord8
  b <- getWord16le
  c <- getWord32le
  return (a,b,c)

main :: IO ()
main = do
  input <- BS.getContents
  print $ runGet deserializeHeader input
