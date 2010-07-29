data Term = Con Float | Div Term Term

data Value a = Result a

data Maybe a = Nothing | Just a

instance Show a => Show (Main.Maybe a) where
	show Main.Nothing = "Main.Nothing"
	show (Main.Just x) = "Main.Just " ++ show x


t1 = Div (Con 6) (Con 3)
t2 = Div (Con 1) (Con 0)

eval2 :: Term -> Main.Maybe Float
eval2 (Con x) = Main.Just x
eval2 (Div t u) = case eval2 t of
	Main.Nothing -> Main.Nothing
	Main.Just x -> case eval2 u of
		Main.Nothing -> Main.Nothing
		Main.Just y -> if y ==0
			then Main.Nothing
			else Main.Just y

