import Data.Bits
import Data.ByteString.Lazy
import Data.Binary.Put
import Data.Word

type Red = Int
type Green = Int
type Blue = Int
type Color = (Red,Green,Blue)


data BitsPerPixel = Bpp8 | Bpp16 | Bpp32

encode :: Color -> BitsPerPixel -> Int-> Int-> Int-> Int-> Int-> Int -> ByteString
encode (r,g,b) bitsPerPixel redMax greenMax blueMax redShift greenShift blueShift = runPut $ do
	case bitsPerPixel of
		Bpp8	-> putWord8 z8
		Bpp16	-> putWord16be z16
		Bpp32	-> putWord32be z32
	where 
		z8  = (fromIntegral $ nr + ng + nb) :: Word8
		z16 = (fromIntegral $ nr + ng + nb) :: Word16
		z32 = (fromIntegral $ nr + ng + nb) :: Word32
		nr = scale r redMax redShift
		ng = scale g greenMax greenShift
		nb = scale b blueMax blueShift
		scale c cm cs = (c * cm `div` 255) `shift` cs
