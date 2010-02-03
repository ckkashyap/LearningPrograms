{---------------------------------------------------------------------

           A HASKELL LIBRARY OF MONADIC PARSER COMBINATORS

                          17th April 1997

               Graham Hutton              Erik Meijer
          University of Nottingham   University of Utrecht

This Haskell 1.3 library is derived from our forthcoming JFP article
"Monadic Parsing in Haskell".  The library also includes a few extra
combinators that were not discussed in the article for reasons of space:

   o force (used to make "many" deliver results lazily);

   o digit, lower, upper, letter, alphanum (useful parsers);

   o ident, nat, int (useful token parsers).

---------------------------------------------------------------------}


module Parselib
   (Parser, item, sat, (+++), string, many, many1, sepby, sepby1,
    chainl, chainl1, char, digit, lower, upper, letter, alphanum,
    symb,  nat, int, token, parse) where


import Monad( MonadPlus( .. ) )


infixr 5 +++

-- Monad of parsers: -------------------------------------------------

newtype Parser a = Parser (String -> [(a,String)])

instance Monad Parser where
   return a      = Parser (\cs -> [(a,cs)])
   p >>= f       = Parser (\cs -> concat [parse (f a) cs' |
                                     (a,cs') <- parse p cs])

--instance MonadZero Parser where

instance MonadPlus Parser where
   p `mplus` q        = Parser (\cs -> parse p cs ++ parse q cs)
   mzero          = Parser (\cs -> [])

-- Other parsing primitives: -----------------------------------------

parse           :: Parser a -> String -> [(a,String)]
parse (Parser p) = p

item            :: Parser Char
item             = Parser (\cs -> case cs of
                                     ""     -> []
                                     (c:cs) -> [(c,cs)])

sat             :: (Char -> Bool) -> Parser Char
sat p            = do {c <- item; if p c then return c else mzero}

-- Efficiency improving combinators: ---------------------------------

force           :: Parser a -> Parser a
force p          = Parser (\cs -> let xs = parse p cs in
                              (fst (head xs), snd (head xs)) : tail xs)

(+++)           :: Parser a -> Parser a -> Parser a
p +++ q          = Parser (\cs -> case parse (p `mplus` q) cs of
                                     []     -> []
                                     (x:xs) -> [x])


-- Recursion combinators: --------------------------------------------

string          :: String -> Parser String
string ""        = return ""
string (c:cs)    = do {char c; string cs; return (c:cs)}

many            :: Parser a -> Parser [a]
many p           = force (many1 p +++ return [])

many1           :: Parser a -> Parser [a]
many1 p          = do {a <- p; as <- many p; return (a:as)}

sepby           :: Parser a -> Parser b -> Parser [a]
p `sepby` sep    = (p `sepby1` sep) +++ return []

sepby1          :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep   = do {a <- p; as <- many (do {sep; p}); return (a:as)}

chainl          :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a    = (p `chainl1` op) +++ return a

chainl1         :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op   = do {a <- p; rest a}
                   where
                      rest a = do {f <- op; b <- p; rest (f a b)}
                               +++ return a

-- Useful parsers: ---------------------------------------------------


isDigit x = x >= '0' && x <= '9'

ord '0' = 48
ord '1' = 49
ord '2' = 50
ord '3' = 51
ord '4' = 52
ord '5' = 53
ord '6' = 54
ord '7' = 55
ord '8' = 56
ord '9' = 57


isLower c = c >= 'a' && c <= 'z'

isUpper c = c >= 'A' && c <= 'Z'

isAlpha c = isUpper c || isLower c

isAlphanum c = isAlpha c || isDigit c


char            :: Char -> Parser Char
char c           = sat (c ==)

digit           :: Parser Int
digit            = do {c <- sat isDigit; return (ord c - ord '0')}

lower           :: Parser Char
lower            = sat isLower

upper           :: Parser Char
upper            = sat isUpper

letter          :: Parser Char
letter           = upper +++ lower

alphanum        :: Parser Char
alphanum         = sat isAlphanum

symb            :: String -> Parser String
symb cs          = token (string cs)


identifier      :: Parser String
identifier       = do {c <- lower; cs <- many alphanum; return (c:cs)}

nat             :: Parser Int
nat              = token natural

natural         :: Parser Int
natural          = digit `chainl1` return (\m n -> 10*m + n)

int             :: Parser Int
int              = token integer

integer         :: Parser Int
integer          = do {char '-'; n <- natural; return (-n)} +++ nat

-- Lexical combinators: ----------------------------------------------

space           :: Parser String
space            = many (sat (\x -> x == ' '))

token           :: Parser a -> Parser a
token p          = do {a <- p; space; return a}

apply           :: Parser a -> String -> [(a,String)]
apply p          = parse (do {space; p})

-- Example parser for arithmetic expressions: ------------------------


data Tree a = Nil | Node a (Tree a) (Tree a)
	deriving (Show)


tdigit           :: Parser (Tree Char)
tdigit            = do {c <- sat isDigit; return (Node c Nil Nil)}

expr  :: Parser (Tree Char)
addop :: Parser (Tree Char -> Tree Char -> Tree Char)
mulop :: Parser (Tree Char -> Tree Char -> Tree Char)

expr   = term   `chainl1` addop
term   = factor `chainl1` mulop
factor = token tdigit +++ do {symb "("; n <- expr; symb ")"; return (n)}

addop  = do {symb "+"; return (Node '+')} +++ do {symb "-"; return (Node '-')}
mulop  = do {symb "*"; return (Node '*')} +++ do {symb "/"; return (Node '/')}

----------------------------------------------------------------------
--
--

data Tree2 leaf node = Leaf leaf    -- for numbers
                    | Node2 node (Tree2 leaf node) (Tree2 leaf node)
  deriving (Show)

type OP = Char
type BetterTree = Tree2 Int OP

tnumB          :: Parser (BetterTree)
tnumB          = do {i <- int; return (Leaf i)}

exprB  :: Parser (BetterTree)
addopB :: Parser (BetterTree -> BetterTree -> BetterTree)
mulopB :: Parser (BetterTree -> BetterTree -> BetterTree)

exprB  = termB  `chainl1` addopB
termB  = factorB `chainl1` mulopB
factorB = token tnumB +++ do {symb "("; n <- exprB; symb ")"; return (n)}

addopB  = do {symb "+"; return (Node2 '+')} +++ do {symb "-"; return
(Node2 '-')}
mulopB  = do {symb "*"; return (Node2 '*')} +++ do {symb "/"; return
(Node2 '/')}

demo2 = parse exprB "10+5*6"

