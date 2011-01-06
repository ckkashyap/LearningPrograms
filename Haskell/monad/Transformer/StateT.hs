module Main where

import Control.Monad.State

type MyState a = StateT Int IO a



stateFun :: MyState String
stateFun = do 
  modify (+1)
  liftIO (putStrLn "Hello!")
  c <- liftIO $ getChar
  x <- get
  liftIO (putStrLn ("Hello!" ++ (show x)))
  if x >= 4 then ((liftIO $ putStrLn "returning\n") >> return "foo")
	  else stateFun

main = do
  (s, n) <- runStateT (stateFun >> stateFun) 0
  putStrLn $ "n: " ++ (show n) ++ " s: " ++ s

