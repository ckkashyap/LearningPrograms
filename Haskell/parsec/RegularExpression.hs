import qualified Control.Monad.Writer as W

data REG = 	EPS
	|	SYM Bool Char
	|	ALT REG REG
	|	SEQ REG REG
	|	REP REG
	deriving (Show)


shift :: Bool -> REG -> Char -> W.Writer [String] REG
shift m EPS c = do
		W.tell $ ["Matching EPS (m = " ++ (show m) ++ ", c = " ++ (show c) ++ ")"]
		return EPS
shift m (SYM _ x) c = do
		W.tell $ ["Matching SYM (m = " ++ (show m) ++ ", c = " ++ (show c) ++ ")"]
		return $ SYM (m && x == c) x
shift m (ALT p q) c = do
	--ALT (shift m p c) (shift m q c)
	pp <- shift m p c
	qq <- shift m q c
	return $ ALT pp qq
shift m (SEQ p q) c = do
	--SEQ (shift m p c) (shift (m && empty p || final p) q c)
	pp <- shift m p c
	qq <- shift (m && empty p || final p) q c
	return $ SEQ pp qq
shift m (REP r) c = do
	--REP (shift (m || final r) r c)
	rr <- shift (m || final r) r c
	return $ REP rr


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



fac :: Int -> Int -> W.Writer [String] Int
fac a 0 = do
	return a
fac a n = do
	W.tell $ ["n = " ++ (show n)]
	v <- fac (n*a) (n-1)
	return v


ff = let (s,v) = W.runWriter (fac 1 10)  in v

val = W.runWriter (shift True evencs 'b')	
