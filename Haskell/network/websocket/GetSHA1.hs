module SHA1 (getSHA1) where
import qualified System.IO as SI
import qualified System.IO.Error as SIE
import qualified System.Process as SP
import qualified Control.Concurrent as CC


getSHA1 :: String -> IO (Either String String)
getSHA1 str = do
      let g = do
            s <- SP.readProcess "openssl" ["dgst", "-sha1"] str
            return (Right s)
      let f = SIE.catchIOError g (\e -> return (Left ("Error running openssl : " ++ (show e))))
      x <- f
      return x



