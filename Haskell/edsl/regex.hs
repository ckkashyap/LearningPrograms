
data Quantifier = ZeroOrMore | OneOrMore | ZeroOrOne

data RegExp = 
     Atom Char
     | Seq RegExp RegExp
     | Quantified RegExp Quantifier


instance Show Quantifier where
         show ZeroOrMore = "*"
         show OneOrMore = "+"
         show ZeroOrOne = "?"         

instance Show RegExp where
         show (Atom c) = [c]
         show (Seq r1 r2) = show r1 ++ show r2
         show (Quantified (Atom a) q) = [a]++(show q)
         show (Quantified r q) = "(" ++ (show r) ++ ")" ++ (show q)

         




fromString :: String -> RegExp
fromString (c:[]) = Atom c
fromString (c:cs) = Seq (Atom c) (fromString cs)

a = Atom 'a'
b = Atom 'b'
ab = Seq a b
ababPlusab = Seq ab (Seq abPlus ab) 
abPlus = Quantified ab OneOrMore



