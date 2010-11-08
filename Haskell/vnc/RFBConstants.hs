module RFBConstants where

import Data.Word

-- Client to Server messages
setPixelFormat			= 0 :: Word8
setEncodings			= 2 :: Word8
framebufferUpdateRequest	= 3 :: Word8
keyEvent			= 4 :: Word8
pointerEvent			= 5 :: Word8
clientCutText			= 6 :: Word8
