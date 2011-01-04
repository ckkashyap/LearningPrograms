module Main where

import Control.Monad.State

type MyState a = StateT Int IO a

stateFun :: MyState String
stateFun = do 
  modify (+100)
  liftIO (putStrLn "Hello!")
  return "foo"

main = do
  (s, n) <- runStateT (stateFun >> stateFun) 0
  putStrLn $ "n: " ++ (show n) ++ " s: " ++ s

