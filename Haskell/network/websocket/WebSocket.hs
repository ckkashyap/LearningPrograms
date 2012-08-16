import qualified Network.Socket as NS
import qualified Control.Concurrent as CC
import qualified System.IO as SI

type HandlerFunc = SI.Handle -> IO ()

main = serveLog "9090" plainHandler

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = NS.withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.  
       addrinfos <- NS.getAddrInfo 
                    (Just (NS.defaultHints {NS.addrFlags = [NS.AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- NS.socket (NS.addrFamily serveraddr) NS.Stream NS.defaultProtocol

       -- Bind it to the address we're listening to
       NS.bindSocket sock (NS.addrAddress serveraddr)

       -- Start listening for connection requests.  Maximum queue size
       -- of 5 connection requests waiting to be accepted.
       NS.listen sock 5

       -- Create a lock to use for synchronizing access to the handler
       lock <- CC.newMVar ()

       -- Loop forever waiting for connections.  Ctrl-C to abort.
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

plainHandler :: HandlerFunc
plainHandler h  = do
    msg <- SI.hGetLine h
    putStrLn $ (show (length msg)) ++ msg
    if (length msg > 1) then plainHandler h else do
      return ()
      putStrLn "Hello World"
