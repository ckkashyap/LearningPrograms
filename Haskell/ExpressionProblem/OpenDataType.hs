data Const = Const Int
data Add l r = Add l r

class Expr x
instance Expr Const
instance (Expr l, Expr r) => Expr (Add l r)

class Expr x => Evaluate x
	where
		evaluate :: x -> Int


instance Evaluate Const
	where
		evaluate (Const i) = i

instance (Evaluate l, Evaluate r) => Evaluate (Add l r)
	where
		evaluate (Add l r) = evaluate l + evaluate r

x = Const 10
y = Const 20
z = Add x y

-- A data extension

data Expr x => Neg x = Neg x

instance Expr x => Expr (Neg x)

instance Evaluate x => Evaluate (Neg x)
	where
		evaluate (Neg x) = 0 - evaluate x

n = Neg z

-- A function extension

class Expr x => PrettyPrint x
	where
		prettyPrint :: x -> String

instance PrettyPrint Const
	where
		prettyPrint (Const i) = show i 

instance (PrettyPrint l, PrettyPrint r) => PrettyPrint (Add l r)
	where
		prettyPrint (Add l r) = prettyPrint l ++ " + " ++ prettyPrint r


instance (PrettyPrint x) => PrettyPrint (Neg x)
	where
		prettyPrint (Neg x) = "-(" ++ prettyPrint x ++ ")"
