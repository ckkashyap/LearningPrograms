module Language where
import Utils

data Expr a
  =  EVar Name                     -- Variables
   | ENum Int                      -- Numbers
   | EConstr Int Int               -- Constructor tag arity
   | EAp (Expr a) (Expr a)         -- Applications
   | ELet                          -- Let(rec) expressions
        IsRec                      --   boolean with True = recursive,
        [(a, Expr a)]              --   Definitions
        (Expr a)                   --   Body of let(rec)
   | ECase                         -- Case expression
        (Expr a)                   --   Expression to scrutinise
        [Alter a]                  --   Alternatives
   | ELam [a] (Expr a)             -- Lambda abstractions
    deriving (Show)

type CoreExpr = Expr Name

type Name = String

type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False

bindersOf :: [(a,b)] -> [a]
bindersOf defns =  [name | (name, rhs) <- defns]

rhssOf        :: [(a,b)] -> [b]
rhssOf defns  =  [rhs  | (name, rhs) <- defns]

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

preludeDefs :: CoreProgram
preludeDefs
  = [ ("I", ["x"], EVar "x"),
      ("K", ["x","y"], EVar "x"),
      ("K1",["x","y"], EVar "y"),
      ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                               (EAp (EVar "g") (EVar "x"))),
      ("compose", ["f","g","x"], EAp (EVar "f")
                                      (EAp (EVar "g") (EVar "x"))),
      ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]

--------------------
-- Prity Printing --
--------------------

pprint :: CoreProgram -> String







pprExpr :: CoreExpr -> String
pprExpr (ENum n) = show n
pprExpr (EVar v) = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprAExpr e2



pprAExpr :: CoreExpr -> String
pprAExpr e 
	| isAtomicExpr e  = pprExpr e
       | otherwise = "(" ++ pprExpr e ++ ")"



mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldll EAp e1 (take n e2s)
                    where
                    e2s = e2 : e2s







iNil     :: Iseq                  -- The empty iseq
iStr     :: String -> Iseq        -- Turn a string into an iseq
iAppend  :: Iseq -> Iseq -> Iseq  -- Append two iseqs
iNewline :: Iseq                  -- New line with indentation
iIndent  :: Iseq -> Iseq          -- Indent an iseq
iDisplay :: Iseq -> String        -- Turn an iseq into a string











iConcat     :: [Iseq] -> Iseq
iInterleave :: Iseq -> [Iseq] -> Iseq

iConcat     = foldr iAppend INil

iInterleave sep = foldr f INil
                  where f x INil = x
                        f x y = x `iAppend` sep `iAppend` y


pprint prog = iDisplay (pprProgram prog)


pprProgram :: CoreProgram -> Iseq
pprProgram defs = iInterleave sep (map pprDef defs)
    where sep = iConcat [ iStr ";", iNewline ]
          pprDef (name, args, e) = iConcat [ iStr name, iStr " ", iInterleave (iStr " ") (map iStr args),
                                             iStr " = ", iIndent (pprExpr e)]



data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq



iNil              = INil
iAppend seq1 seq2 = IAppend seq1 seq2
iStr str             = IStr str


iIndent seq = seq
iNewline = IStr "\n"


flatten :: [Iseq] -> String

iDisplay seq = flatten [seq]



flatten [] = ""


flatten (INil : seqs) = flatten seqs


flatten (IStr s : seqs) = s ++ (flatten seqs)


flatten (IAppend seq1 seq2 : seqs)  = flatten (seq1 : seq2 : seqs)























iNum :: Int -> Iseq
iNum n = iStr (show n)

iFWNum :: Int -> Int -> Iseq
iFWNum width n
  = iStr (space (width - length digits) ++ digits)
    where
    digits = show n


iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
             where
             lay_item (n, seq)
               = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]











clex :: String -> [Token]


syntax :: [Token] -> CoreProgram


parse :: String -> CoreProgram
parse = syntax . clex
-- In Gofer I propose to compose this with some function
-- CoreProgram -> String, which will illustrate some sort of
-- execution machine, and then give this composition to catWith
-- from my utils




type Token = String           -- A token is never empty


clex (c:cs) | isWhiteSpace c = clex cs


clex (c:cs) | isDigit c = num_token : clex rest_cs
             where
             num_token = c : takeWhile isDigit cs
             rest_cs   = dropWhile isDigit cs



clex (c:cs) | isAlpha c = var_tok : clex rest_cs
             where
             var_tok = c : takeWhile isIdChar cs
             rest_cs = dropWhile isIdChar cs



clex (c:cs) = [c] : clex cs


clex [] = []

isDigit c
       | c >= '0' && c <= '9' = True
       | otherwise = False

isAlpha c
	| c >='a' && c <= 'z' = True
       | c >= 'A' && c <= 'Z' = True
       | otherwise = False

isIdChar, isWhiteSpace :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')
isWhiteSpace c = c `elem` " \t\n"



twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]






type Parser a = [Token] -> [(a, [Token])]



pLit :: String -> Parser String


pLit s (tok:toks) | s == tok = [(s, toks)]
                  | otherwise = []
pLit s []         = []


pVar :: Parser String
pVar []         = []



pAlt :: Parser a -> Parser a -> Parser a


pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)


pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")



pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c


pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2) | (v1,toks1) <- p1 toks,
                               (v2,toks2) <- p2 toks1]


pGreeting :: Parser (String, String)
pGreeting = pThen mk_pair pHelloOrGoodbye pVar
            where
            mk_pair hg name = (hg, name)







pZeroOrMore :: Parser a -> Parser [a]


pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting


pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])


pEmpty :: a -> Parser a


pOneOrMore :: Parser a -> Parser [a]



pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreeting) `pApply` length


pApply :: Parser a -> (a -> b) -> Parser b



pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]




pSat :: (String -> Bool) -> Parser String





keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]


pNum :: Parser Int







syntax = take_first_parse . pProgram
         where
         take_first_parse ((prog,[]) : others) = prog
         take_first_parse (parse     : others) = take_first_parse others
         take_first_parse other                = error "Syntax error"


pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr













data PartialExpr = NoOp | FoundOp Name CoreExpr


pExpr1c :: Parser PartialExpr
pExpr1c = (pThen FoundOp (pLit "|") pExpr1) `pAlt` (pEmpty NoOp)


pExpr1 :: Parser CoreExpr
pExpr1 = pThen assembleOp pExpr2 pExpr1c

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2







