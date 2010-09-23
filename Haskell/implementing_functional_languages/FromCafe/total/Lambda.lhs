\chapter{Lambda Lifting\index{lambda lifting}}

> module Lambda where
> import Utils
> import Language

\begin{important}
each level of the tree is a pair, whose first
component is the annotation, and whose second component is the
abstract syntax tree node.
\end{important}

> type AnnExpr a b = (b, AnnExpr' a b)

> data AnnExpr' a b = AVar Name
>                   | ANum Int
>                   | AConstr Int Int
>                   | AAp (AnnExpr a b) (AnnExpr a b)
>                   | ALet Bool [AnnDefn a b] (AnnExpr a b)
>                   | ACase (AnnExpr a b) [AnnAlt a b]
>                   | ALam [a] (AnnExpr a b)
>                     deriving Show

> type AnnDefn a b = (a, AnnExpr a b)

> type AnnAlt a b  = (Int, [a], (AnnExpr a b))

> type AnnProgram a b = [(Name, [a], AnnExpr a b)]

%****************************************************************
%*                                                              *
        \section{Mark~1: A simple lambda lifter\index{lambda lifter!Mark 1}}
        \label{ll:mk1}
%*                                                              *
%****************************************************************

\subsection{Implementing a simple lambda lifter}
\label{ll:mk1-imp}

We are now ready to develop a simple lambda lifter. It will take a
@coreProgram@ and return an equivalent @coreProgram@ in which there
are no occurrences of the @ELam@ constructor.

> lambdaLift :: CoreProgram -> CoreProgram

The lambda lifter works in three passes:
\begin{itemize}
\item
First, we annotate every node in the expression with its free variables.
This is used by the following pass to decide which extra parameters
to add to a lambda abstraction.  The @freeVars@ function has type

> freeVars :: CoreProgram -> AnnProgram Name (Set Name)

\item
Second, the function @abstract@ abstracts
from each lambda abstraction $@\@x_1\ldots x_n@.@e$ its
free variables $v_1\ldots v_m$,
replacing the lambda abstraction with
an expression of the form
\[
(@let sc = \@v_1 \ldots v_m ~ x_1 \ldots x_n@.@~e~@in sc@)~v_1 \ldots v_m
\]
We could use a direct application of the lambda abstraction to the free
variables, but we need to give the new supercombinator a name, so we take
the first step here by always giving it the name @sc@.
For example, the lambda abstraction
\begin{verbatim}
        (\x. y*x + y*z)
\end{verbatim}
would be transformed to
\begin{verbatim}
        (let sc = (\y z x.  y*x + y*z) in sc) y z
\end{verbatim}
@abstract@ has the type signature:

> abstract :: AnnProgram Name (Set Name) -> CoreProgram

Notice, from the type signature, that @abstract@ removes the free variable
information, which is no longer required.

\item
Now we traverse the program giving a unique name to each variable.
This will have the effect of making unique all the @sc@ variables
introduced by the previous pass.  Indeed, the sole purpose of
introducing the extra @let@ expressions in the first place was to give
each supercombinator a name which could then be made unique.  As a
side effect, all other names in the program will be made unique, but
this does not matter, and it will turn out to be useful later.

> rename :: CoreProgram -> CoreProgram

\item
Finally, @collectSCs@
collects all the supercombinator definitions into a single list, and
places them at the top level of the program.

> collectSCs :: CoreProgram -> CoreProgram

\end{itemize}

The lambda lifter itself is the composition of these three functions:

> lambdaLift = collectSCs . rename . abstract . freeVars

To make it easier to see what is happening we define the a function @runS@
(the `@S@' stands for `simple') to integrate the parser and printer:

> runS = pprint . lambdaLift . parseCore


\subsection{Free variables}

The core of the free-variable pass is function @freeVars_e@ which has type

> freeVars_e :: (Set Name)                   -- Candidates for free variables
>               -> CoreExpr                  -- Expression to annotate
>               -> AnnExpr Name (Set Name)   -- Annotated result

Its first argument is the set of local variables which are in scope;
these are the possible free variables.  The second argument is the
expression to be annotated, and the result is the annotated
expression.  The main function @freeVars@ just runs down the list of
supercombinator definitions, applying @freeVars_e@ to each:

> freeVars prog = [ (name, args, freeVars_e (setFromList args) body)
>                 | (name, args, body) <- prog
>                 ]

The @freeVars_e@ function runs over the expression recursively; in the
case of numbers there are no free variables, so this is what is
returned in the annotated expression.

> freeVars_e lv (ENum k)      = (setEmpty, ANum k)

\par
In the case of a variable, we check to see whether it is in the set of
candidates to decide whether to return the empty set or a singleton set:

> freeVars_e lv (EVar v) | setElementOf v lv = (setSingleton v, AVar v)
>                        | otherwise         = (setEmpty, AVar v)

\par
The case for applications is straightforward: we first annotate the
expression @e1@ with its free variables, then annotate @e2@, returning
the union of the two sets of free variables as the free variables of
@EAp e1 e2@.

> freeVars_e lv (EAp e1 e2)
>  = (setUnion (freeVarsOf e1') (freeVarsOf e2'), AAp e1' e2')
>    where e1'            = freeVars_e lv e1
>          e2'            = freeVars_e lv e2

In the case of a lambda abstractions we need to add the @args@ to the
local variables passed in, and subtract them from the free variables
passed out:

> freeVars_e lv (ELam args body)
>  = (setSubtraction (freeVarsOf body') (setFromList args), ALam args body')
>    where body'          = freeVars_e new_lv body
>          new_lv         = setUnion lv (setFromList args)

\par
The equation for @let(rec)@ expressions has rather a lot of plumbing,
but is quite straightforward. The local variables in scope that are
passed to the body is @body_lv@; the set of local variables passed to
each right-hand side is @rhs_lv@. Next we annotate each right-hand
side with its free variable set, giving @rhss'@, from this we can
construct the annotated definitions: @defns'@. The annotated body of
the @let(rec)@ is @body'@. The free variables of the definitions is
calculated to be @defnsFree@, and those of the body are @bodyFree@.

> freeVars_e lv (ELet is_rec defns body)
>  = (setUnion defnsFree bodyFree, ALet is_rec defns' body')
>    where binders        = bindersOf defns
>          binderSet      = setFromList binders
>          body_lv        = setUnion lv binderSet
>          rhs_lv | is_rec    = body_lv
>                 | otherwise = lv
>
>          rhss'          = map (freeVars_e rhs_lv) (rhssOf defns)
>          defns'         = zip binders rhss'
>          freeInValues   = setUnionList (map freeVarsOf rhss')
>          defnsFree | is_rec    = setSubtraction freeInValues binderSet
>                    | otherwise = freeInValues
>          body'          = freeVars_e body_lv body
>          bodyFree       = setSubtraction (freeVarsOf body') binderSet

We postpone dealing with @case@ and constructor expressions:

> freeVars_e lv (ECase e alts) = freeVars_case lv e alts
> freeVars_e lv (EConstr t a) = error "freeVars_e: no case for constructors"
> freeVars_case lv e alts = error "freeVars_case: not yet written"

@freeVarsOf@ and @freeVarsOf_alt@ are simple auxiliary functions:

> freeVarsOf :: AnnExpr Name (Set Name) -> Set Name
> freeVarsOf (free_vars, expr) = free_vars

> freeVarsOf_alt :: AnnAlt Name (Set Name) -> Set Name
> freeVarsOf_alt (tag, args, rhs)
>  = setSubtraction (freeVarsOf rhs) (setFromList args)

\subsection{Generating supercombinators}
\label{ll:abstract}

The next pass merely replaces each lambda abstraction, which is now
annotated with its free variables, with a new abstraction (the
supercombinator) applied to its free variables.

> abstract = map f
>     where f (sc_name, args, rhs) = (sc_name, args, abstract_e rhs)

As usual, we define an auxiliary function @abstract_e@ to do most of
the work:

> abstract_e :: AnnExpr Name (Set Name) -> CoreExpr

It takes an expression annotated with the free variable information
and returns an expression with each lambda abstraction replaced by a
new abstraction applied to the free variables. There is little to say
about the first four cases, they just recursively abstract each
expression.

> abstract_e (free, AVar v)    = EVar v
> abstract_e (free, ANum k)    = ENum k
> abstract_e (free, AAp e1 e2) = EAp (abstract_e e1) (abstract_e e2)
> abstract_e (free, ALet is_rec defns body)
>  =  ELet is_rec [ (name, abstract_e body) | (name, body) <- defns]
>                   (abstract_e body)

Notice the way that the free-variable information is discarded
by the pass, since it is no longer required.

The final case we show is the heart of the @abstract_e@ function.
First we create a list of free variables: @fvList@. We recall that
there is no ordering implicit in a set; the function @setToList@ has
induced an ordering on the elements, but we do not much care what order
this is. Next we make a new supercombinator. This involves
\begin{enumerate}
\item applying @abstract_e@ to the body of the lambda expression; and

\item augmenting the argument list, by prefixing the original one with
the free-variable list.
\end{enumerate}
Next, to allow the @collectSCs@ pass to detect this new
supercombinator, we wrap it into a @let@ expression.  Finally, we apply
the new supercombinator to each free variable in turn.

> abstract_e (free, ALam args body)
>  = foldl EAp sc (map EVar fvList)
>    where
>    fvList = setToList free
>    sc = ELet False [("sc",sc_rhs)] (EVar "sc")
>    sc_rhs = ELam (fvList ++ args) (abstract_e body)

@case@ expressions and constructors are deferred:

> abstract_e (free, ACase e alts) = abstract_case free e alts
> abstract_e (free, AConstr t a) = error "abstract_e: no case for Constr"
> abstract_case free e alts = error "abstract_case: not yet written"

It is worth observing that @abstract_e@ treats the two expressions
\linebreak[3]
@(ELam args1 (ELam args2 body))@ and @(ELam (args1++args2) body)@
\linebreak[3]
differently.  In the former case, the two abstractions will be treated
separately, generating two supercombinators, while in the latter only
one supercombinator is produced.  It is clearly advantageous to merge
directly nested @ELam@s before performing lambda lifting.  This is
equivalent to the $\eta$-abstraction optimisation noted by
\cite{HughesThesis}.

\subsection{Making all the variables unique\index{renaming pass, of lambda lifter}}

Next, we need to make each variable so that all the @sc@ variables
introduced by @abstract@ are unique.  The auxiliary function,
@rename_e@, takes an environment mapping old names to new names, a
name supply and an expression.  It returns a depleted name supply and
a new expression.

> rename_e :: ASSOC Name Name                   -- Binds old names to new
>             -> NameSupply                     -- Name supply
>             -> CoreExpr                       -- Input expression
>             -> (NameSupply, CoreExpr)         -- Depleted supply and result

Now we can define @rename@ in terms of @rename_e@, by applying the latter
to each supercombinator definition, plumbing the name supply along with
@mapAccuml@.

> rename prog
>  = snd (mapAccuml rename_sc initialNameSupply prog)
>    where
>    rename_sc ns (sc_name, args, rhs)
>     = (ns2, (sc_name, args', rhs'))
>       where
>       (ns1, args', env) = newNames ns args
>       (ns2, rhs') = rename_e env ns1 rhs

\par
The function @newNames@ takes a name supply and a list of names as its
arguments.  It allocates a new name for each old one from the name supply,
returning the depleted name supply, a list of new names and an association
list mapping old names to new ones.

> newNames :: NameSupply -> [Name] -> (NameSupply, [Name], ASSOC Name Name)
> newNames ns old_names
>  = (ns', new_names, env)
>    where
>    (ns', new_names) = getNames ns old_names
>    env = zip old_names new_names

The definition of @rename_e@ is now straightforward, albeit dull.
When we meet a variable, we look it up in the environment.  For top-level
functions and built-in functions (such as @+@) we will find no substitution
for it in the environment, so we just use the existing name:

> rename_e env ns (EVar v)      = (ns, EVar (aLookup env v v))

\par
Numbers and applications are easy.

> rename_e env ns (ENum n)      = (ns, ENum n)
> rename_e env ns (EAp e1 e2)
>  = (ns2, EAp e1' e2')
>    where
>    (ns1, e1') = rename_e env ns e1
>    (ns2, e2') = rename_e env ns1 e2

When we meet an @ELam@ we need to invent new names for the arguments,
using @newNames@, and augment the environment with the mapping
returned by @newNames@.

> rename_e env ns (ELam args body)
>  = (ns1, ELam args' body')
>    where
>    (ns1, args', env') = newNames ns args
>    (ns2, body') = rename_e (env' ++ env) ns1 body

@let(rec)@ expressions work similarly:

> rename_e env ns (ELet is_rec defns body)
>  = (ns3, ELet is_rec (zip2 binders' rhss') body')
>    where
>    (ns1, body') = rename_e body_env ns body
>    binders = bindersOf defns
>    (ns2, binders', env') = newNames ns1 binders
>    body_env = env' ++ env
>    (ns3, rhss') = mapAccuml (rename_e rhsEnv) ns2 (rhssOf defns)
>    rhsEnv | is_rec    = body_env
>           | otherwise = env

\par
We leave @case@ expressions as an exercise:

> rename_e env ns (EConstr t a) = error "rename_e: no case for constructors"
> rename_e env ns (ECase e alts) = rename_case env ns e alts
> rename_case env ns e alts = error "rename_case: not yet written"

\subsection{Collecting supercombinators\index{collecting
supercombinators pass, of lambda lifter}}

Finally, we have to name the supercombinators and collect them
together.  The main function, @collectSCs_e@, therefore has to return
the collection of supercombinators it has found, as well as the
transformed expression.

> collectSCs_e :: CoreExpr -> ([CoreScDefn], CoreExpr)

@collectSCs@ is defined using @mapAccuml@ to do all the plumbing:

> collectSCs prog
>  = concat (map collect_one_sc prog)
>    where

Eliminate redundant supercombinators. Add a special case when rhs is of the form "let _ = \_ . _".
This special case only matches with supercombinators whose rhs is a lambda abstraction with no free variables.
We concat the supercombinator's arguments with the rhs' arguments, and replace the supercombinator's body with the rhs' body.
Run a complex example like "f = \x. (let a = \u. u+10 in a x)" to see how this works!

>    collect_one_sc (sc_name, args, rhs@(ELet _ [(_, ELam _ _)] _)) = (sc_name, args ++ args', rhs') : scs
>        where (sc':scs, _) = collectSCs_e rhs
>              (_, args', rhs') = sc'
>    collect_one_sc (sc_name, args, rhs) = (sc_name, args, rhs') : scs
>      where (scs, rhs') = collectSCs_e rhs

\par
The code for @collectSCs_e@ is now easy to write.  We just apply
@collectSCs_e@ recursively to the sub-expressions, collecting up the
supercombinators thus produced.

> collectSCs_e (ENum k)      = ([], ENum k)
> collectSCs_e (EVar v)      = ([], EVar v)
> collectSCs_e (EAp e1 e2)   = (scs1 ++ scs2, EAp e1' e2')
>                              where
>                              (scs1, e1') = collectSCs_e e1
>                              (scs2, e2') = collectSCs_e e2

> collectSCs_e (ELam args body) = (scs, ELam args body')
>                                 where
>                                 (scs, body') = collectSCs_e body
> collectSCs_e (EConstr t a) = ([], EConstr t a)
> collectSCs_e (ECase e alts)
>  = (scs_e ++ scs_alts, ECase e' alts')
>    where
>    (scs_e, e') = collectSCs_e e
>    (scs_alts, alts') = mapAccuml collectSCs_alt [] alts
>    collectSCs_alt scs (tag, args, rhs) = (scs++scs_rhs, (tag, args, rhs'))
>                                          where
>                                          (scs_rhs, rhs') = collectSCs_e rhs

\par
The case for @let(rec)@ is the interesting one.  We need to
process the definitions recursively and then split them into two
groups: those of the form $v ~@= \@ args @.@~e$ (the
supercombinators), and the others (the non-supercombinators).  The
supercombinators are returned as part of the supercombinator list, and
a new @let(rec)@ is formed from the remaining non-supercombinators:

> collectSCs_e (ELet is_rec defns body)
>  = (local_scs ++ rhss_scs ++ body_scs, mkELet is_rec non_scs' body')
>    where
>    (rhss_scs,defns') = mapAccuml collectSCs_d [] defns
>
>    scs'     = [(name,rhs) | (name,rhs) <- defns',  isELam rhs ]
>    non_scs' = [(name,rhs) | (name,rhs) <- defns',  not (isELam rhs)]
>    local_scs = [(name,args,body) | (name,ELam args body) <- scs']
>    (body_scs, body') = collectSCs_e body

Eliminating redundant local definitions.

>    collectSCs_d scs (name, rhs@(ELet _ [(_, ELam _ _)] _)) = (scs ++ rhs_scs, (name, ELam args rhs'))
>        where (sc':rhs_scs, _) = collectSCs_e rhs
>              (_, args, rhs') = sc'
>    collectSCs_d scs (name,rhs) = (scs ++ rhs_scs, (name, rhs'))
>                                  where
>                                  (rhs_scs, rhs') = collectSCs_e rhs

The auxiliary function @isELam@ tests for an @ELam@ constructor; it is
used to identify supercombinators.

> isELam :: Expr a -> Bool
> isELam (ELam args body) = True
> isELam other            = False

The @mkELet@ function just builds an @ELet@ expression:

> mkELet _ [] body = body
> mkELet is_rec defns body = ELet is_rec defns body

