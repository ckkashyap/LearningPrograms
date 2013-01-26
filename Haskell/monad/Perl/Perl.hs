module Perl where
import Control.Monad.State


type MyType = State String String

emit :: String -> MyType
emit str = do
	x <- get
	let n = x ++ str ++ "\n"
	put n
	return n
	

fetch :: MyType -> String
fetch something = let (a, s) = runState something "" in s
	


test3 :: MyType
test3 = do
	emit "hello"
	emit "world"
	return "dingo"



test1 :: State Int (Int,Int)
test1 = do
            a <- get
            modify (+1)
            b <- get
            return (a,b)


test2 :: State String (String,String)
test2 = do
            a <- get
            modify (++"1")
            b <- get
            return (a,b)

go1 = evalState test1 0
go2 = evalState test2 "0" 

