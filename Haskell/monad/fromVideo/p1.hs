data Term = Con Float | Div Term Term

data Value a = Result a

instance Show a => Show (Value a) where
	show (Result x) = "Result: " ++ show x


t1 = Div (Con 6) (Con 3)
t2 = Div (Con 1) (Con 0)

eval1 :: Term -> Value Float
eval1 (Con x) = Result x
eval1 (Div t u) = Result (x/y)
		where
			Result x = eval1 t
			Result y = eval1 u

