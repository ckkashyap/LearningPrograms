
data Quantifier = ZeroOrMore | OneOrMore | ZeroOrOne | NonGreedyZeroOrMore | NonGreedyOneOrMore

data RegularExpression = 
     Atom Char
     | Sequence RegularExpression RegularExpression
     | Quantified RegularExpression Quantifier
     | Group RegularExpression


instance Show Quantifier where
         show ZeroOrMore = "*"
         show OneOrMore = "+"
         show ZeroOrOne = "?"
         show NonGreedyOneOrMore = "+?"
         show NonGreedyZeroOrMore = "*?"

instance Show RegularExpression where
         show (Atom c) = if isMeta then '\\':[c] else [c]
              where isMeta = elem c "()*+?\\"
         show (Sequence r1 r2) = show r1 ++ show r2
         show (Quantified (Atom a) q) = [a]++(show q)
         show (Quantified r q) = "(" ++ (show r) ++ ")" ++ (show q)
         show (Group r) = "(" ++ (show r) ++ ")"

stringToRegularExpression :: String -> RegularExpression
stringToRegularExpression (c:[]) = Atom c
stringToRegularExpression (c:cs) = Sequence (Atom c) (stringToRegularExpression cs)

listToSequence :: [RegularExpression] -> RegularExpression
listToSequence (r:[]) = r
listToSequence (r:rs) = Sequence r (listToSequence rs)

oneOrMore r = Quantified r OneOrMore
zeroOrMore r = Quantified r ZeroOrMore
zeroOrOne r = Quantified r ZeroOrOne
nonGreedyOneOrMore r = Quantified r NonGreedyOneOrMore
nonGreedyZeroOrMore r = Quantified r NonGreedyZeroOrMore


a = Atom 'a'

hello = stringToRegularExpression "hello"

world = stringToRegularExpression "world"

someStr = stringToRegularExpression "(1*)"

oneOrMoreWorld = oneOrMore world

re = listToSequence [hello, oneOrMoreWorld, someStr]





