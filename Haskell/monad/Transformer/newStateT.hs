module Main where

import Control.Monad.State

data Dingo = Dingo String deriving (Show)

app s (Dingo str)= Dingo (str ++ s)

type MyState a = StateT Dingo IO a

stateFun :: MyState String
stateFun = do 
  modify (app "hello")
  liftIO (putStrLn "Hello!")
  return "foo"

main = do
  (s, n) <- runStateT (stateFun >> stateFun) (Dingo "")
  putStrLn $ "n: " ++ (show n) ++ " s: " ++ s

