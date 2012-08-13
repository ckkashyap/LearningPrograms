import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO

type HandlerFunc = Handle -> IO ()

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.  
       addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol

       -- Bind it to the address we're listening to
       bindSocket sock (addrAddress serveraddr)

       -- Start listening for connection requests.  Maximum queue size
       -- of 5 connection requests waiting to be accepted.
       listen sock 5

       -- Create a lock to use for synchronizing access to the handler
       lock <- newMVar ()

       -- Loop forever waiting for connections.  Ctrl-C to abort.
       procRequests lock sock

    where
          -- | Process incoming connection requests
          procRequests :: MVar () -> Socket -> IO ()
          procRequests lock mastersock = 
              do (connsock, clientaddr) <- accept mastersock
                 forkIO $ procMessages lock connsock clientaddr
                 procRequests lock mastersock

          -- | Process incoming messages
          procMessages :: MVar () -> Socket -> SockAddr -> IO ()
          procMessages lock connsock clientaddr =
              do connhdl <- socketToHandle connsock ReadWriteMode
                 hSetBuffering connhdl LineBuffering
                 handlerfunc connhdl
                 
                 
-- A simple handler that prints incoming packets

plainHandler :: HandlerFunc
plainHandler h  = do
    msg <- hGetLine h
    hPutStrLn h msg