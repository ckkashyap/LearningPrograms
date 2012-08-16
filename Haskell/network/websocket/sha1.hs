import qualified System.IO as SI
import qualified System.IO.Error as SIE
import qualified System.Process as SP
import qualified Control.Concurrent as CC


getSha1 :: String -> IO (Either String String)
getSha1 str = do
      let g = do
            s <- SP.readProcess "openssl" ["dgst", "-sha1"] str
            return (Right s)
      let f = SIE.catchIOError g (\e -> return (Left ("Error running openssl : " ++ (show e))))
      x <- f
      return x


main = do
  x<-getSha1 "x3JJHMbDL1EzLkh9GBhXDw==258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  putStrLn (show x)


