import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..), mallocByteString, memcpy,
                                 unsafeCreate)
import Data.ByteString.Unsafe (unsafeIndex)
import Data.Word (Word8, Word16, Word32)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, minusPtr, plusPtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff)
import System.IO.Unsafe (unsafePerformIO)


copy s d 0 = d
copy s@(PS sfp soff slen) d@(PS dfp doff dlen) n = unsafePerformIO $ do
	withForeignPtr sfp $ \sptr  -> do
		withForeignPtr dfp $ \dptr -> do
			let o = (slen - n)
			let sp = plusPtr sptr o :: Ptr Word8
			let dp = plusPtr dptr o :: Ptr Word8
			v<-peek $! sp :: IO Word8
			poke dp (v+1)
			return $! copy s d (n-1)

	return d

input = B.pack $ [65..100]

f :: ByteString -> ByteString
f bs@(PS sfp soff slen) = unsafePerformIO $ do
	dfp <- mallocByteString slen
--	withForeignPtr dfp $ \ptr -> do
--		let p0 = plusPtr ptr 0 :: Ptr Word8
--		let p1 = plusPtr ptr 1 :: Ptr Word8
--		let p2 = plusPtr ptr 2 :: Ptr Word8
--		let p3 = plusPtr ptr 3 :: Ptr Word8
--		let p4 = plusPtr ptr 4 :: Ptr Word8
--		poke p0 65 
--		poke p1 66 
--		poke p2 67 
--		poke p3 68 
--		poke p4 69 
	return $! copy (PS sfp 0 slen) (PS dfp 0 slen) slen
	

output = ""

main = do
	putStrLn (show input)
	putStrLn (show $ f input)
	
