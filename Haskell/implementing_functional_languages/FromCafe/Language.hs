 
  module Language where
  import Utils
 
  data Expr a
    =  EVar Name                     -- Variables
     | ENum Int                  -- Numbers
     | EConstr Int Int       -- Constructor tag arity
     | EAp (Expr a) (Expr a)         -- Applications
     | ELet                          -- Let(rec) expressions
          IsRec                      --   boolean with True = recursive,
          [(a, Expr a)]              --   Definitions
          (Expr a)                   --   Body of let(rec)
     | ECase                         -- Case expression
          (Expr a)                   --   Expression to scrutinise
          [Alter a]                  --   Alternatives
     | ELam [a] (Expr a)             -- Lambda abstractions
       deriving Show
 
 
 
 
  type CoreExpr = Expr Name
 
 
 
  type Name = String
 
 
 
  type IsRec = Bool
 
 
  bindersOf :: [(a,b)] -> [a]
  bindersOf defns =  [name | (name, rhs) <- defns]
 
  rhssOf        :: [(a,b)] -> [b]
  rhssOf defns  =  [rhs  | (name, rhs) <- defns]
 
 
  type Alter a = (Int, [a], Expr a)
  type CoreAlt = Alter Name
 
 
  isAtomicExpr :: Expr a -> Bool
  isAtomicExpr (EVar v) = True
  isAtomicExpr (ENum n) = True
  isAtomicExpr (EConstr _ 0) = True
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
 
 
  iStr     :: String -> Iseq        -- Turn a string into an iseq
  iAppend  :: Iseq -> Iseq -> Iseq  -- Append two iseqs
  iNewline :: Iseq                  -- New line with indentation
  iIndent  :: Iseq -> Iseq          -- Indent an iseq
  iDisplay :: Iseq -> String        -- Turn an iseq into a string
 
  opNames  = ["+","-","*","/","<",">","&","|"] ++ ["==", "~=", ">=", "<=", "->"]
 
  pprExpr :: CoreExpr -> Iseq
  pprExpr (ENum n) = iStr (show n)
  pprExpr (EVar v) = iStr v
                     
  pprExpr (EAp (EAp (EVar op) e1) e2)
    | op `elem` opNames = iConcat [ pprAExpr e1, iStr " ", iStr op, iStr " ", pprAExpr e2 ]
  pprExpr (EAp e1 e2) = (pprAExpr e1) `iAppend` (iStr " ") `iAppend` (iIndent $ pprAExpr e2)
 
  pprExpr (EConstr tag arity) = iConcat $ map iStr ["Pack{", show tag, ",", show arity, "}"]
 
 
 
  pprExpr (ELet isrec defns expr)
    = iConcat [  iStr keyword, iNewline,
                 iStr "  ",iIndent (pprDefns defns), iNewline,
                 iStr "  in ", iIndent $ pprExpr expr ]
      where
      keyword | not isrec = "let"
              | isrec = "letrec"
 
  pprExpr (ECase e alt)
      = iConcat [ iStr "case ", iIndent (pprAExpr e), iStr " of", iNewline,
                  iStr "  ", iIndent pprCases]
        where pprCases = iInterleave sep (map pprCase alt)
              sep = iConcat [ iStr ";", iNewline ]
              pprCase (tag, vars, expr) = iConcat [ iStr "<", iStr $ show tag, iStr "> ",
                                                    iInterleave (iStr " ") (map iStr vars),
                                                    iStr " -> ", iIndent (pprExpr expr)]
 
  pprExpr (ELam vars e)
      = iConcat [ iStr "\\ ", iInterleave (iStr " ") (map iStr vars), iStr ". ", iIndent $ pprExpr e]
 
  pprDefns :: [(Name,CoreExpr)] -> Iseq
  pprDefns defns = iInterleave sep (map pprDefn defns)
                   where
                   sep = iConcat [ iStr ";", iNewline ]
 
  pprDefn :: (Name, CoreExpr) -> Iseq
  pprDefn (name, expr)
    = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]
 
 
  iConcat     :: [Iseq] -> Iseq
  iInterleave :: Iseq -> [Iseq] -> Iseq
 
 
  iConcat     = foldr iAppend INil
 
  iInterleave sep = foldr f INil
                    where f x INil = x
                          f x y = x `iAppend` sep `iAppend` y
 
 
  pprint prog = iDisplay (pprProgram prog)
 
  pprAExpr :: CoreExpr -> Iseq
  pprAExpr e
      | isAtomicExpr e = pprExpr e
      | otherwise = iStr "(" `iAppend` pprExpr e `iAppend` iStr ")"
 
  pprProgram :: CoreProgram -> Iseq
  pprProgram defs = iInterleave sep (map pprDef defs)
      where sep = iConcat [ iStr ";", iNewline ]
            pprDef (name, args, e) = iConcat [ iStr name, iStr " ", iInterleave (iStr " ") (map iStr args),
                                               iStr " = ", iIndent (pprExpr e)]
 
 
 
 
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
 
 
  flatten col ((INewline, indent) : seqs)
   = '\n' : (space indent) ++ (flatten indent seqs)
 
 
 
  flatten col ((IIndent seq, indent) : seqs)
   = flatten col ((seq, col) : seqs)
 
 
  flatten col ((INil, _) : seqs) = flatten col seqs
     
  flatten col ((IStr s, _) : seqs) = s ++ (flatten col' seqs)
                                   where col' = col + length s
                                   
  flatten col ((IAppend seq1 seq2, n) : seqs) = flatten col $ (seq1, n):(seq2, n):seqs
 
  flatten _ [] = ""
 
 
 
 
 
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
 
 
 
 
