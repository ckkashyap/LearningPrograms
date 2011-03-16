data Exp = 
	  LitDbl Double
	| LitStr String
	| LitBool Bool
	| Apply Func [Exp]
	| Var Id
	deriving (Show)

type Func = String
type Id = String

data E a = E Exp

instance Num (E Double) where
	E x + E y = E (Apply "+" [x,y])
	E x - E y = E (Apply "-" [x,y])
	E x * E y = E (Apply "*" [x,y])
	fromInteger i = E (LitDbl (fromInteger i))


instance Show (E Double) where
	show (E (LitDbl x)) = show x
	show (E (Apply s (a:b:_))) = "Apply " ++ s ++ "[" ++ (show a) ++ ", " ++ (show b) ++ "]"
	show _		= "DINGO"

instance Eq (E Double) where
	(E x) == (E y) = False


 
