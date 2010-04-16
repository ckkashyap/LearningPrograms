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
     deriving (Text)



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





 pprint :: CoreProgram -> String













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








 pprDefns :: [(Name,CoreExpr)] -> Iseq
 pprDefns defns = iInterleave sep (map pprDefn defns)
                  where
                  sep = iConcat [ iStr ";", iNewline ]

 pprDefn :: (Name, CoreExpr) -> Iseq
 pprDefn (name, expr)
   = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]


 iConcat     :: [Iseq] -> Iseq
 iInterleave :: Iseq -> [Iseq] -> Iseq



 pprint prog = iDisplay (pprProgram prog)








 iNil              = INil
 iAppend seq1 seq2 = IAppend seq1 seq2
 iStr str             = IStr str



















 data Iseq = INil
           | IStr String
           | IAppend Iseq Iseq
           | IIndent Iseq
           | INewline

 iIndent seq = IIndent seq
 iNewline    = INewline


 flatten :: Int                       -- Current column; 0 for first column
             -> [(Iseq, Int)]         -- Work list
             -> String                -- Result


 iDisplay seq = flatten 0 [(seq,0)]













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














 isIdChar, isWhiteSpace :: Char -> Bool
 isIdChar c = isAlpha c || isDigit c || (c == '_')
 isWhiteSpace c = c `elem` " \t\n"



 twoCharOps :: [String]
 twoCharOps = ["==", "~=", ">=", "<=", "->"]






 type Parser a = [Token] -> [(a, [Token])]



 pLit :: String -> Parser String







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


 pLit s = pSat (== s)



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







