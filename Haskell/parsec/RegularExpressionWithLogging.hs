import qualified Control.Monad.Writer as W

data REG = 	EPS
	|	SYM Bool Char
	|	ALT REG REG
	|	SEQ REG REG
	|	REP REG
	deriving (Show)


shift :: Bool -> REG -> Char -> W.Writer [String] REG
shift m EPS c = do
		W.tell $ ["START"]
		W.tell $ ["Matching EPS (m = " ++ (show m) ++ ", c = " ++ (show c) ++ ")"]
		W.tell $ ["STOP"]
		return EPS
shift m (SYM _ x) c = do
		W.tell $ ["START"]
		W.tell $ ["Matching SYM (m = " ++ (show m) ++ ", c = " ++ (show c) ++ ")"]
		let ret = SYM (m && x == c) x 
		W.tell $ ["RET -> " ++ (show ret)]
		W.tell $ ["STOP"]
		return ret
shift m r@(ALT p q) c = do
	W.tell $ ["START"]
	W.tell $ ["Matching " ++ (show r) ++ " (m = " ++ (show m) ++ ", c = " ++ (show c) ++ ")"]
	pp <- shift m p c
	qq <- shift m q c
	let ret = ALT pp qq
	W.tell $ ["RET -> " ++ (show ret)]
	W.tell $ ["STOP"]
	return ret
shift m r@(SEQ p q) c = do
	W.tell $ ["START"]
	W.tell $ ["Matching " ++ (show r) ++ " (m = " ++ (show m) ++ ", c = " ++ (show c) ++ ")"]
	pp <- shift m p c
	qq <- shift (m && empty p || final p) q c
	let ret = SEQ pp qq
	W.tell $ ["RET -> " ++ (show ret)]
	W.tell $ ["STOP"]
	return ret
shift m rr@(REP r) c = do
	W.tell $ ["START"]
	W.tell $ ["Matching " ++ (show rr) ++ " (m = " ++ (show m) ++ ", c = " ++ (show c) ++ ")"]
	rr <- shift (m || final r) r c
	let ret = REP rr
	W.tell $ ["RET -> " ++ (show ret)]
	W.tell $ ["STOP"]
	return ret


empty :: REG -> Bool
empty EPS = True
empty (SYM _ _) = False
empty (ALT p q) = empty p || empty q
empty (SEQ p q) = empty p && empty q
empty (REP r) = True

final :: REG -> Bool
final EPS = False
final (SYM b _) = b
final (ALT p q) = final p || final q
final (SEQ p q) = final p && empty q || final q
final (REP r) = final r


nocs = REP (ALT (SYM False 'a') (SYM False 'b'))
onec = SEQ nocs (SYM False 'c')
evencs = SEQ (REP (SEQ onec onec)) nocs



fac :: Int -> W.Writer [String] Int
fac 0 = return (1)

fac n = do
	W.tell $ ["n = " ++ (show n)]
	v <- fac (n-1)
	return (v*n)


ff = let (s,v) = W.runWriter (fac 10)  in v

val = W.runWriter (shift True evencs 'b')	

output = mapM print log
	where
		(_,log) = val



mlist :: [Int]
mlist = do
	return 1
	
