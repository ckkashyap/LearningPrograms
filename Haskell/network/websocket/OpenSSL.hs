module OpenSSL (getSHA1,encodeBase64,decodeBase64) where
import qualified System.IO as SI
import qualified System.IO.Error as SIE
import qualified System.Process as SP
import qualified Control.Concurrent as CC



openSSL args str = do
      let g = do
            s <- SP.readProcess "openssl" args str
            return (Right s)
      SIE.catchIOError g (\e -> return (Left ("Error running openssl : " ++ (show e))))
      


getSHA1 :: String -> IO (Either String String)
getSHA1 = openSSL ["dgst", "-sha1"]

encodeBase64 :: String -> IO (Either String String)
encodeBase64 = openSSL ["enc", "-e", "-a"]

decodeBase64 :: String -> IO (Either String String)
decodeBase64 = openSSL ["enc", "-d", "-a"]
