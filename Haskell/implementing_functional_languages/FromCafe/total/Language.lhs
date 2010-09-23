The data type of Core-language expression, @expr@, is defined as follows:

> module Language where
> import Utils
> import Text.Parsec hiding(space)
> import qualified Text.Parsec.Token as P
> import Text.Parsec.String
> import Text.Parsec.Language
> import Text.Parsec.Expr
> import Control.Applicative hiding ((<|>), many)

> data Expr a
>   =  EVar Name                     -- Variables
>    | ENum Int                  -- Numbers
>    | EConstr Int Int       -- Constructor tag arity
>    | EAp (Expr a) (Expr a)         -- Applications
>    | ELet                          -- Let(rec) expressions
>         IsRec                      --   boolean with True = recursive,
>         [(a, Expr a)]              --   Definitions
>         (Expr a)                   --   Body of let(rec)
>    | ECase                         -- Case expression
>         (Expr a)                   --   Expression to scrutinise
>         [Alter a]                  --   Alternatives
>    | ELam [a] (Expr a)             -- Lambda abstractions
>      deriving Show


We choose to parameterise the data type of @expr@ with respect to its
\stressD{binders}.  A binder is the name used at the binding
occurrence of a variable; that is, on the left-hand side of a @let(rec)@
definition, or in a lambda abstraction.
The declaration
can be read `An @expr@ of @*@ is either an @EVar@ containing a @name@,
or \ldots, or an @ELam@ containing a list of values of
type @*@ and an @expr@ of @*@'.

For the most of the book we always use @name@ in these binding positions,
so we use a \stressD{type synonym} to define the type of @coreExpr@,
which is the type we will normally use:

> type CoreExpr = Expr Name

The ability to use types other than @name@ in binding positions
is only used in Chapter~\ref{sect:lambda-lift}.

Variables are represented by an @EVar@ constructor containing the variable's
name.  A variable's name is represented simply by a list of characters, which
we express using another type synonym:

> type Name = String

Constructors are identified by their arity and tag, as described in
Section~\ref{sect:lang:constructors}.

@let@ and @letrec@ expressions are represented by an @ELet@ constructor
containing: a flag of type @isRec@ to
distinguish the recursive case from the non-recursive one; a list of
definitions; and the expression which is the body of the @let(rec)@.
We choose to represent @isRec@ as a boolean variable, and we define the two
boolean values as follows:

> type IsRec = Bool

Each definition is just a pair of the variable name being bound and the
expression to which it is bound.
We define two useful functions which each take a list of definitions:
@bindersOf@ picks out the list of variables bound by the definitions,
and @rhssOf@ (short for
`right-hand sides of') extracts the list of right-hand sides
to which they are bound.

> bindersOf :: [(a,b)] -> [a]
> bindersOf defns =  [name | (name, rhs) <- defns]

> rhssOf        :: [(a,b)] -> [b]
> rhssOf defns  =  [rhs  | (name, rhs) <- defns]

@case@ expressions have an expression to analyse, and  a list of
alternatives.  Each alternative contains a tag, a list of the bound
variables and  the expression to the right of the arrow.

> type Alter a = (Int, [a], Expr a)
> type CoreAlt = Alter Name

We take the opportunity to define a useful function on expressions, a
boolean-valued function, @isAtomicExpr@,
which identifies `atomic' expressions\index{atomic expressions}; that is,
expressions with no internal structure:

> isAtomicExpr :: Expr a -> Bool
> isAtomicExpr (EVar v) = True
> isAtomicExpr (ENum n) = True
> isAtomicExpr (EConstr _ 0) = True
> isAtomicExpr e        = False

Finally, a Core-language
program is just a list of supercombinator definitions:

> type Program a = [ScDefn a]
> type CoreProgram = Program Name

A supercombinator definition contains the name of the supercombinator,
its arguments and its body:

> type ScDefn a = (Name, [a], Expr a)
> type CoreScDefn = ScDefn Name

The argument list might be empty, in the case of a supercombinator
with no arguments.

We conclude with a small example. Consider the following small program.
\begin{verbatim}
        main = double 21 ;
        double x = x+x
\end{verbatim}
This program is represented by the following Miranda expression, of type
@coreProgram@:
\begin{verbatim}
        [("main",   [],    (EAp (EVar "double") (ENum 21))),
         ("double", ["x"], (EAp (EAp (EVar "+") (EVar "x")) (EVar "x")))
        ]
\end{verbatim}

%****************************************************************
%*                                                              *
%*                           Prelude                            *
%*                                                              *
%****************************************************************

\section{A small standard prelude}
\label{sect:prelude}

Miranda has a \stressD{standard prelude} which contains definitions of
various useful functions (such as @map@, @foldr@ and so on)
which are always available.  We will do the same for the Core language,
by providing the following standard definitions:
\begin{verbatim}
        I x = x ;
        K  x y = x ;
        K1 x y = y ;
        S f g x = f x (g x) ;
        compose f g x = f (g x) ;
        twice f = compose f f
\end{verbatim}
This `standard prelude' is necessarily rather small, because we want it
to work for {\em all\/} of our implementations, including the most primitive
ones which will lack arithmetic and facilities for manipulating data
structures.  All that is available in the simplest implementations
is function application!

The following definition for @preludeDefs@, which will be used
throughout the book, embodies these definitions:

> preludeDefs :: CoreProgram
> preludeDefs
>   = [ ("I", ["x"], EVar "x"),
>       ("K", ["x","y"], EVar "x"),
>       ("K1",["x","y"], EVar "y"),
>       ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
>                                (EAp (EVar "g") (EVar "x"))),
>       ("compose", ["f","g","x"], EAp (EVar "f")
>                                       (EAp (EVar "g") (EVar "x"))),
>       ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]

%****************************************************************
%*                                                              *
%*              Pretty printer                                  *
%*                                                              *
%****************************************************************

\section{A pretty-printer for the Core language}
\label{sect:pretty} \index{pretty-printer} \index{Core language!pretty printer}

What we require is a `pretty-printing' function @pprint@, with type

> pprint :: CoreProgram -> String

\subsection{An abstract data type for pretty-printing}

> iStr     :: String -> Iseq        -- Turn a string into an iseq
> iAppend  :: Iseq -> Iseq -> Iseq  -- Append two iseqs
> iNewline :: Iseq                  -- New line with indentation
> iIndent  :: Iseq -> Iseq          -- Indent an iseq
> iDisplay :: Iseq -> String        -- Turn an iseq into a string

> opNames  = ["+","-","*","/","<",">","&","|"] ++ ["==", "~=", ">=", "<=", "->"]

> pprExpr :: CoreExpr -> Iseq
> pprExpr (ENum n) = iStr (show n)
> pprExpr (EVar v) = iStr v
>                    
> pprExpr (EAp (EAp (EVar op) e1) e2)
>   | op `elem` opNames = iConcat [ pprAExpr e1, iStr " ", iStr op, iStr " ", pprAExpr e2 ]
> pprExpr (EAp e1 e2) = (pprAExpr e1) `iAppend` (iStr " ") `iAppend` (iIndent $ pprAExpr e2)
>
> pprExpr (EConstr tag arity) = iConcat $ map iStr ["Pack{", show tag, ",", show arity, "}"]

What are the differences between an @iseq@ and a list of characters?
Firstly, we
aim to produce an implementation of @iAppend@ which does not have the
unexpected quadratic behaviour of list append.
Secondly, @iseq@ provides new operations @iIndent@ and @iNewline@
which will be useful
for controlling indentation.
The idea is that @iIndent@ indents
its argument to line up with the current column;
it should work even if its
argument spreads over many lines, and itself
contains calls to @iIndent@.
@iNewline@ stands for a newline
followed by a number of spaces determined by
the current level of indentation.

As an example of how @iIndent@ and @iNewline@
might be used, let us extend @pprExpr@ to handle
@let@ and @letrec@ expressions:

> pprExpr (ELet isrec defns expr)
>   = iConcat [  iStr keyword, iNewline,
>                iStr "  ",iIndent (pprDefns defns), iNewline,
>                iStr "  in ", iIndent $ pprExpr expr ]
>     where
>     keyword | not isrec = "let"
>             | isrec = "letrec"
>
> pprExpr (ECase e alt)
>     = iConcat [ iStr "case ", iIndent (pprAExpr e), iStr " of", iNewline,
>                 iStr "  ", iIndent pprCases]
>       where pprCases = iInterleave sep (map pprCase alt)
>             sep = iConcat [ iStr ";", iNewline ]
>             pprCase (tag, vars, expr) = iConcat [ iStr "<", iStr $ show tag, iStr "> ",
>                                                   iInterleave (iStr " ") (map iStr vars),
>                                                   iStr " -> ", iIndent (pprExpr expr)]
>
> pprExpr (ELam vars e)
>     = iConcat [ iStr "\\ ", iInterleave (iStr " ") (map iStr vars), iStr ". ", iIndent $ pprExpr e]

> pprDefns :: [(Name,CoreExpr)] -> Iseq
> pprDefns defns = iInterleave sep (map pprDefn defns)
>                  where
>                  sep = iConcat [ iStr ";", iNewline ]

> pprDefn :: (Name, CoreExpr) -> Iseq
> pprDefn (name, expr)
>   = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]

To make the definitions more legible, we have used
two new functions, @iConcat@ and @iInterleave@, with the types

> iConcat     :: [Iseq] -> Iseq
> iInterleave :: Iseq -> [Iseq] -> Iseq

@iConcat@ takes a list of @iseq@s and
uses @iAppend@ to concatenate them into a single @iseq@.  @iInterleave@ is
similar to @iConcat@ except that it interleaves a specified @iseq@ between
each adjacent pair.

> iConcat     = foldr iAppend INil

> iInterleave sep = foldr f INil
>                   where f x INil = x
>                         f x y = x `iAppend` sep `iAppend` y

In general, all our pretty-printing functions will return an
@iseq@, and we apply @iDisplay@ just once at the top level, to the @iseq@
representing the entire thing we want to display:

> pprint prog = iDisplay (pprProgram prog)

> pprAExpr :: CoreExpr -> Iseq
> pprAExpr e
>     | isAtomicExpr e = pprExpr e
>     | otherwise = iStr "(" `iAppend` pprExpr e `iAppend` iStr ")"

> pprProgram :: CoreProgram -> Iseq
> pprProgram defs = iInterleave sep (map pprDef defs)
>     where sep = iConcat [ iStr ";", iNewline ]
>           pprDef (name, args, e) = iConcat [ iStr name, iStr " ", iInterleave (iStr " ") (map iStr args),
>                                              iStr " = ", iIndent (pprExpr e)]

\subsection{Implementing @iseq@}

Now we come to the {\em implementation\/} of the @iseq@ type.  We begin
by making an implementation that ignores all indentation.
To implement the abstract data type we must say what type is used to
represent an @iseq@:

The general idea of this particular representation
is to postpone all the work until the eventual call of @iDisplay@.

> iAppend seq1 seq2 = IAppend seq1 seq2
> iStr str             = IStr str

All the interest lies in the operation @iDisplay@ which turns an @iseq@ into
a list of characters.  The goal is that it should only take time linear in the
size of the @iseq@.  It turns out to be convenient to define @iDisplay@ in
terms of a more general function, @flatten@:

The function @flatten@ takes a {\em list\/} of @iseqRep@s, and returns the
result of concatenating each of the @iseqRep@s in the list.
The reason for having this list is that is allows us to accumulate a list
of pending work, as we will soon see.

\subsection{Layout and indentation\index{indentation}}

So far we have only given a rather trivial interpretation to the @iIndent@
operation, and we now turn to improving it.
In the same spirit as before, we first expand the @iseqRep@ type
with an extra two constructors, @IIndent@ and @INewline@, and redefine
their operations to use these constructors:

> data Iseq = INil
>           | IStr String
>           | IAppend Iseq Iseq
>           | IIndent Iseq
>           | INewline
>
> iIndent seq = IIndent seq
> iNewline    = INewline

We must then make @flatten@ more powerful.  Firstly, it needs
to keep track of the current column,
and secondly, its work-list must consist
of @(iseq, num)@ pairs, where the number gives the indentation required for
the corresponding @iseq@:

> flatten :: Int                       -- Current column; 0 for first column
>             -> [(Iseq, Int)]         -- Work list
>             -> String                -- Result

We need to change @iDisplay@ to initialise @flatten@ appropriately:

> iDisplay seq = flatten 0 [(seq,0)]

The interesting case for @flatten@ is when we deal with @INewline@, because
this is where we need to perform indentation\footnote{%
@spaces@ is a standard Miranda function which returns a list of a specified
number of space characters.
}:

> flatten col ((INewline, indent) : seqs)
>  = '\n' : (space indent) ++ (flatten indent seqs)

Notice that the recursive call to flatten has a current-column argument
of @indent@ since we have now moved on to a new line and added @indent@
spaces.

The @IIndent@ case simply sets the current indentation from the
current column:

> flatten col ((IIndent seq, indent) : seqs)
>  = flatten col ((seq, col) : seqs)

\begin{exercise}
The pretty-printer will go wrong if a newline character @'\n'@ is
embedded in a string given to @IStr@. Modify @iStr@ to check for this,
replacing the newline character by a use of @INewline@.
\end{exercise}

> flatten col ((INil, _) : seqs) = flatten col seqs
>    
> flatten col ((IStr s, _) : seqs) = s ++ (flatten col' seqs)
>                                  where col' = col + length s
>                                  
> flatten col ((IAppend seq1 seq2, n) : seqs) = flatten col $ (seq1, n):(seq2, n):seqs
>
> flatten _ [] = ""

\subsection{Operator precedence\index{operator precedence}}

Would you prefer to see the expression
\begin{verbatim}
        x + y > p * length xs
\end{verbatim}
or the fully parenthesised version?
\begin{verbatim}
        (x + y) > (p * (length xs))
\end{verbatim}
The easiest way to achieve this is to give @pprExpr@ an extra argument
which indicates the precedence level of its context, and then
use this to decide whether to add parentheses around the expression it
produces.
(The function @pprAExpr@ now becomes redundant.)
\begin{exercise}
Make these changes to @pprExpr@ and test them.
\end{exercise}

\subsection{Other useful functions on @iseq@}

@iNum@ maps a number to an @iseq@ and @iFWNum@ does the same except that
the result is left-padded with spaces to a specified width:

> iNum n = iStr (show n)

> iFWNum :: Int -> Int -> Iseq
> iFWNum width n
>   = iStr (space (width - length digits) ++ digits)
>     where
>     digits = show n

(If the number is wider than the width required, a negative number will
be passed to @spaces@, which then returns the empty list.  So the net
effect is to return a field just wide enough to contain the number.)
@iLayn@ lays out a list, numbering the items and putting a  newline
character after each, just as the standard function @layn@ does.

> iLayn :: [Iseq] -> Iseq
> iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
>              where
>              lay_item (n, seq)
>                = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]


%****************************************************************
%*                                                              *
%*              Parser                                          *
%*                                                              *
%****************************************************************

\section{A parser for the Core language}
\label{sect:parser} \indexD{parser}
\index{Core language!parser}

> parseCore :: String -> CoreProgram
> parseCore input = case parse pProgram "" input of
>                     Left v -> error $ show v
>                     Right v -> v

\subsection{Separate Lexical analysis}

Now the lexical analysis itself.  It should throw away white space (blanks,
tabs, newlines):

 lexer :: Parser [Token]
 lexer = whitespace >> sepEndBy token whitespace

 clex input = let (Right v) = P.parse lexer "" input
              in v

A comment is introduced by a double vertical bar, @||@, and extend to
the end of the line.

 whitespace = skipMany $ (P.space >> return ()) <|> (string "||" >> skipMany (noneOf "\n"))

We define such two-character operators by giving a list of them:

 token :: Parser Token
 token = many1 digit -- numbers
     <|> (:) <$> letter <*> many (alphaNum <|> char '_') -- variables
     <|> (try $ choice $ map string twoCharOps) -- two-character operators
     <|> (:[]) <$> anyChar -- single characters

\subsection{Integrated Lexical analysis}

> lexer     = P.makeTokenParser coreDef

> coreDef    = emptyDef
>                { P.commentLine    = "||"
>                , P.identStart     = letter
>                , P.identLetter    = alphaNum <|> char '_'
>                , P.reservedNames  = ["let","in","letrec","case","of","Pack"]
>                }


> parens          = P.parens lexer    
> braces          = P.braces lexer
> angles          = P.angles lexer
> semiSep1        = P.semiSep1 lexer
> semiSep         = P.semiSep lexer
> semi            = P.semi lexer
> whiteSpace      = P.whiteSpace lexer    
> symbol          = P.symbol lexer    
> identifier      = P.identifier lexer    
> reserved        = P.reserved lexer    
> natural         = P.natural lexer
> comma           = P.comma lexer


\subsection{Parsing the Core language}

\par
The beauty of our parsing tools is that {\em we can write
parsers by merely transliterating the
grammar into Miranda}.  For example, consider the productions for $program$
and $sc$ in
Figure~\ref{fig:core-syntax}:
\begin{center}
$\begin{array}{lcll}
program & \rightarrow & sc_1 @;@~\ldots @;@~sc_n & (n \geq 1) \\
sc & \rightarrow & var ~ var_1 \ldots var_n ~@=@~ expr &(n\geq 0)
\end{array}$
\end{center}
We can transliterate these directly into Miranda:

> pProgram :: Parser CoreProgram
> pProgram = whiteSpace >> (semiSep1 pSc <* eof)

> pSc :: Parser CoreScDefn
> pSc = do
>   var <- identifier
>   args <- many identifier
>   symbol "="
>   expr <- pExpr
>   return (var, args, expr)

> pExpr :: Parser CoreExpr
> pExpr = pLet
>     <|> pCase
>     <|> pLambda
>     <|> buildExpressionParser operators pExpr'
>     where pExpr' = mkApChain <$> many1 pAexpr
>           mkApChain :: [CoreExpr] -> CoreExpr
>           mkApChain [atom] = atom
>           mkApChain (f:args) = foldl EAp f args

> operators =
>     [ [ op "*"  AssocLeft, op "/"  AssocLeft ]
>     , [ op "+"  AssocLeft, op "-"  AssocLeft ]
>     , [ op "==" AssocNone, op "~=" AssocNone, op "<="  AssocNone
>       , op "<" AssocNone, op ">="  AssocNone, op ">" AssocNone ]
>     , [ op "&" AssocRight ] 
>     , [ op "|" AssocRight ] 
>     ]
>     where
>       op name assoc   = Infix (do{ var <- try (symbol name)
>                                   ; return (\x y -> EAp (EAp (EVar var) x) y)
>                                   }) assoc

> pLet = let l = (reserved "let" >> return False) <|> (reserved "letrec" >> return True)
>            in ELet <$> l <*> pDefns <*> (reserved "in" *> pExpr)

> pDefns = semiSep1 pDefn
>     where pDefn = do
>             v <- identifier
>             symbol "="
>             e <- pExpr
>             return (v,e)

> pCase = reserved "case" >> ECase <$> pExpr <*> (reserved "of" *> pAlts)

We must be careful to only treat something beginning with '<' as a
case branch!  Conflicts exist because ';' is used as a separator for
both case branches and top-level definitions.

> pAlts = (:) <$> pAlt <*> rest
>     where pAlt = do {
>                    n <- angles natural;
>                    v <- many identifier;
>                    symbol "->";
>                    e <- pExpr;
>                    return (fromInteger n, v, e)
>                  }
>           tryAlt = try (semi >> Just <$> pAlt)
>                    <|> return Nothing
>           rest = do
>                    x <- tryAlt
>                    case x of
>                      Just y -> (y:) <$> rest
>                      Nothing -> return []

> pLambda = symbol "\\" >> ELam <$> (many1 identifier) <*> (symbol "." *> pExpr)

> pAexpr = EVar <$> identifier
>      <|> (ENum . fromInteger) <$> natural
>      <|> (reserved "Pack" >> (braces $ (EConstr . fromInteger) <$> natural <*> (comma *> (fromInteger <$> natural))))
>      <|> parens pExpr

> testParser = getContents >>= (putStrLn . pprint . parseCore)
