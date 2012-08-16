import qualified Network.Socket as NS
import qualified Control.Concurrent as CC
import qualified System.IO as SI
import qualified OpenSSL as OpenSSL

type HandlerFunc = SI.Handle -> IO ()

main = serveLog "9090" plainHandler

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = NS.withSocketsDo $
    do 
       addrinfos <- NS.getAddrInfo 
                    (Just (NS.defaultHints {NS.addrFlags = [NS.AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       sock <- NS.socket (NS.addrFamily serveraddr) NS.Stream NS.defaultProtocol

       NS.bindSocket sock (NS.addrAddress serveraddr)

       NS.listen sock 5

       lock <- CC.newMVar ()

       procRequests lock sock

    where
          -- | Process incoming connection requests
          procRequests :: CC.MVar () -> NS.Socket -> IO ()
          procRequests lock mastersock = 
              do (connsock, clientaddr) <- NS.accept mastersock
                 CC.forkIO $ procMessages lock connsock clientaddr
                 procRequests lock mastersock

          -- | Process incoming messages
          procMessages :: CC.MVar () -> NS.Socket -> NS.SockAddr -> IO ()
          procMessages lock connsock clientaddr =
              do connhdl <- NS.socketToHandle connsock SI.ReadWriteMode
                 SI.hSetBuffering connhdl SI.LineBuffering
                 handlerfunc connhdl
                 
                 
-- A simple handler that prints incoming packets

readRawHeader :: SI.Handle -> IO [String]
readRawHeader h = do
  line' <- SI.hGetLine h
  let line = take (length line' -1) line'
  rest <- if length line > 0 then readRawHeader h else return []
  return (line:rest)

getHeaderValue :: String -> [String] -> String
getHeaderValue _ [] = []               
getHeaderValue h (s:ss) = if rightHeader then extractKey else getHeaderValue h ss where
  headerField = h ++ ": "
  lengthOfHeaderField = length headerField
  extractKey = drop lengthOfHeaderField s
  rightHeader = (take lengthOfHeaderField s) == headerField


getResponseHeaders :: String -> String -> [String]
getResponseHeaders protocol key =
  [
      "HTTP/1.1 101 Switching Protocols"
    , "Upgrade: websocket"
    , "Connection: Upgrade"
    , "Sec-WebSocket-Accept: " ++ key
    , "Sec-WebSocket-Protocol: " ++ protocol
  ]


plainHandler :: HandlerFunc
plainHandler h  = do
    hdr <- readRawHeader h
    let key = getHeaderValue "Sec-WebSocket-Key" hdr
    let protocol = getHeaderValue "Sec-WebSocket-Protocol" hdr
    let totalKey = key ++ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
    (Right sha1) <- OpenSSL.getSHA1 totalKey
    (Right base64EncodedSHA1) <- OpenSSL.encodeBase64 (drop 9 (take 40 sha1))
    putStrLn (show base64EncodedSHA1)
    let respHdr = getResponseHeaders protocol (take (length base64EncodedSHA1 - 1) base64EncodedSHA1)
    foldr (>>) (return ())  $  map (SI.hPutStrLn h) $ map (\e -> e ++ "\r") respHdr
    foldr (>>) (return ())  $  map putStrLn $ map (\e -> e ++ "\r") respHdr  

