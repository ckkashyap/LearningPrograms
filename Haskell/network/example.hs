module Main where

import Network.Server
import Network.Socket
import Control.Monad
import System.IO


-- |start a TCP (stream) echo server on IPv6 and IPv4

main1 :: IO ()
main1 = let 
		port = 60000
		ip6 = Server (SockAddrInet6 port 0 (0, 0, 0, 0x1) 0xe) Stream echo
		ip =  Server (SockAddrInet port (ipAddress (127, 0, 0, 1))) Stream echo

	in serveMany Nothing [ip6, ip] >>= waitFor


-- |this starts an echo server on privileged port 80 and then stops being root
main2 :: IO ()
main2 = do
	running <- serveOne (Just $ UserWithDefaultGroup "nobody") server
	putStrLn "server is accepting connections!!!"
	waitFor running

	where server = Server (SockAddrInet 8000 iNADDR_ANY) Stream echo

main :: IO ()
main = main2


-- |the simple echo server routine
echo :: ServerRoutine
echo (h,n,p) = do
	l <- hGetLine h
	hPutStrLn h l
	hFlush h
	when ((l!!0)/='q') (echo (h,n,p))


-- |beCurious tries to do something that only root should be able to do
beCurious :: ServerRoutine
beCurious (h,_,_) = readFile "/etc/shadow" >>= hPutStr h
