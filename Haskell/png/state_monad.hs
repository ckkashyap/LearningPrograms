import qualified  Data.ByteString as B
import Control.Monad.State

width = 256
height = 256
bytesInImage = width * height * 3
blankImage =  B.pack $ take bytesInImage (repeat  0)

type Color = (Int,Int,Int)
setPixel1 :: B.ByteString -> Int -> Int -> Color ->  B.ByteString
setPixel1 image x y (r,g,b) = B.concat [beforePixel,  pixel, afterPixel]
         where
                 beforePixel = B.take before image
                 afterPixel = B.drop (before+3)  image
                 pixel=B.pack [(fromIntegral r),(fromIntegral g),(fromIntegral  b)]
                 before = (y * width * 3) + (x * 3) -  3



setPixel' :: Int -> Int -> Color ->  B.ByteString -> ((), B.ByteString)
setPixel'  x y (r,g,b)  image = ((), B.concat [beforePixel, pixel, afterPixel])
         where
                 beforePixel = B.take before image
                 afterPixel = B.drop (before+3)  image
                 pixel=B.pack [(fromIntegral r),(fromIntegral g),(fromIntegral  b)]
                 before = (y * width * 3) + (x * 3) -  3

setPixel x y rgb = State $ setPixel' x y rgb


drawPixels = do
	setPixel 5 10 (255, 255, 255)
	setPixel 100 100 (255, 0, 0)
	setPixel 101 100 (255, 0, 0)
	setPixel 101 101 (255, 0, 0)
	setPixel 100 101 (255, 0, 0)
	setPixel 90 2 (255, 255,  255)
	
modifiedImage = execState drawPixels  blankImage



main = do
        putStrLn "P6"
        putStrLn (  (show width) ++ " " ++ (show height) )
        putStrLn "255"
	B.putStr modifiedImage
        -- Set a red  pixel at 100 100
        --B.putStr (setPixel 100 100 (255,0,0) blankImage)



