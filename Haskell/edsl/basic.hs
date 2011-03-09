import Control.Monad.State 


type LineNumber = Int
type Command = String

data Statement = Statement LineNumber [(LineNumber,Command)] deriving (Show)

data Basic = Basic Statement deriving (Show)



emptyProgram = Basic (Statement 10 [])

pp :: String -> State Basic ()
pp str = do
	(Basic (Statement ln a)) <- get
	let basicString = "PRINT \"" ++ str ++ "\""
	let st = Statement (ln + 10) (a++[(ln,basicString)])
	put (Basic st)
	return ()

inp :: String -> State Basic ()
inp str = do
	(Basic (Statement ln a)) <- get
	let basicString = "INPUT \"" ++ str ++ "\""
	let st = Statement (ln + 10) (a++[(ln,basicString)])
	put (Basic st)
	return ()


fun :: State Basic ()
fun = do
	pp "hello world"
	inp "AA"
	pp "KASHYAP"
	return ()
	
res = let (_,v) = runState fun emptyProgram in v

main = putStrLn (show res)

