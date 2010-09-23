> module Template where
> import Language
> import Utils

\chapter{Template instantiation}
\label{sect:template}

This chapter introduces the simplest possible implementation of
a functional language: a graph reducer based
on \stressD{template instantiation}.

\subsection{Structure of the implementation}

It should take a filename, run the program therein, and print
out the results, which might be either the final result or some kind
of execution trace.

> runProg :: FilePath -> IO String

\begin{enumerate}
\item
Translate the program
into a form suitable for execution.  The @compile@ function,
which performs this task, takes a program and
produces the initial state of the template instantiation machine:

> compile :: CoreProgram -> TiState

@tiState@ is the type of the state of the template instantiation machine.
(The prefix `@ti@' is short for template instantiation.)
\item
Execute the program, by performing repeated state transitions until
a final state is reached.  The result is a list of all the states passed
through; from this we can subsequently
either extract the final state, or get a trace
of all the states.
For the present we will restrict ourselves to programs which return
a number as their result, so we call this execution function @eval@.

> eval :: TiState -> [TiState]

\item
Format the results for printing.  This is done by the function @showResults@,
which selects which information to print, and formats it into a
list of characters.

> showResults :: [TiState] -> [Char]

\end{enumerate}
The function @run@ is just the composition of these four functions:

> runProg f = readFile f >>= (return . showResults . eval . compile . parseCore)

\subsection{The compiler}

\subsubsection{Data types}

The compiler produces the initial state of the machine, which has
type @tiState@, so the next thing to do is to define how machine states
are represented, using a type synonym\index{type synonym}:

> type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

\begin{itemize}
\item
The {\em spine stack\/}\index{spine stack}
is just a stack of {\em heap addresses\/}:

> type TiStack = [Addr]

We choose to represent the stack as a list.  The elements of the stack
are members of the abstract data type @addr@ defined in the @utils@
module (Appendix~\ref{sect:heap}).

\item
The {\em heap\/} is represented by the @heap@ abstract data type defined in
the @utils@ module.  We have to say what the heap contains, namely objects
of type @node@ (yet to be defined):

> type TiHeap = Heap Node

Heap @node@s are represented by the following algebraic data type
declaration, which corresponds to the list of possibilities given in
Section~\ref{sect:templ:transition-rules}:

> data Node = NAp Addr Addr                     -- Application
>             | NSupercomb Name [Name] CoreExpr -- Supercombinator
>             | NNum Int                    -- A number
>             | NInd Addr                       -- Indirection
>             | NPrim Name Primitive              -- Primitive
>             | NData Int [Addr]                  -- Tag, list of components
>             | NMarked Node                      -- Marked node

\item
The {\em globals\/} component associates each
supercombinator name with
the address of a heap node containing its definition:

> type TiGlobals = ASSOC Name Addr

\item
The @tiStats@ component of the state is not mentioned in the
transition rules, but we will use it to collect run-time performance
statistics\index{statistics} on what the machine does.
So that we can easily change what statistics are collected,
we will make it an abstract type.  To begin with, we will record only
the number of steps taken:

> tiStatInitial  :: TiStats
> tiStatIncSteps :: TiStats -> TiStats
> tiStatGetSteps :: TiStats -> Int

The implementation is rather simple:

> type TiStats = Int
> tiStatInitial    = 0
> tiStatIncSteps s = s+1
> tiStatGetSteps s = s

A useful function @applyToStats@ applies a given function to the
statistics\index{statistics}
component of the state:

> applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
> applyToStats stats_fun (stack, dump, heap, sc_defs, stats)
>  = (stack, dump, heap, sc_defs, stats_fun stats)

\end{itemize}
This completes our definition of the data types involved.

\subsubsection{The compiler itself}
\label{sect:ti:compiler}
\label{sect:mapAccuml-example}

The business of the compiler is to take a program, and from
it create the initial state of the machine:

> compile program
>  = (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
>    where
>    sc_defs = program ++ preludeDefs ++ parseCore extraPreludeDefs
>
>    (initial_heap, globals) = buildInitialHeap sc_defs
>
>    initial_stack = [address_of_main]
>    address_of_main = aLookup globals "main" (error "main is not defined")

\par
Let us consider each of the definitions in the @where@ clause in turn.
The first, @sc_defs@, is just a list
of all the supercombinator definitions involved in the program.
Recall that @preludeDefs@ was defined in Section~\ref{sect:prelude}
to be the list of standard supercombinator definitions which
are always included in every program.  @extraPreludeDefs@ is a list of
any further standard functions we may want to add:

> -- We seem to assume that "and" and "or" are functions here.
> -- But they are defined as operators in the Language module?
> extraPreludeDefs = "False = Pack{1,0};\n\
>                    \True = Pack{2,0};\n\
>                    \and x y = if x y False;\n\
>                    \or x y = if x True y;\n\
>                    \xor x y = if x (not y) y;\n\
>                    \not x = if x False True\n"

\par
The second definition uses an auxiliary function, @buildInitialHeap@, to
construct an initial heap containing an @NSupercomb@ node for
each supercombinator, together with an association list @globals@ which
maps each supercombinator name onto the address of its node.

Lastly, @initial_stack@ is defined to contain just one item, the address
of the node for the supercombinator @main@, obtained from @globals@.

Now we need to consider the definition of @buildInitialHeap@,
which is a little
tricky.  We need to do something for each element of the list @sc_defs@,
but what makes it awkward is that the `something' involves heap allocation.
Since each heap allocation produces a new heap, we need to find a way of
passing the heap along from one element of @sc_defs@ to the next.
This process starts with the empty heap, @hInitial@
(Appendix~\ref{sect:heap}).

We encapsulate this idea in a higher-order function\index{higher-order function}
@mapAccuml@, which
we will use quite a lot in this book.  @mapAccuml@ takes three
arguments: $f$, the `processing function'; $acc$, the
`accumulator'; and a list $[x_1, \ldots, x_n]$.  It takes each
element of the input list, and applies $f$ to it and the current
accumulator\index{accumulator}.  $f$ returns a pair of results, an
element of the result list and a new value for the accumulator.
@mapAccuml@ passes the accumulator along from one call of $f$ to the
next, and eventually returns a pair of results: $acc'$, the final
value of the accumulator; and the result list $[y_1, \ldots, y_n]$.
Figure~\ref{fig:mapAccuml} illustrates this plumbing.  The definition
of @mapAccuml@ is given in Appendix~\ref{sect:util-funs}.
\begin{figure} %\centering
\input{map_acc.tex}
\caption{A picture of $@mapAccuml@~f~acc~[x_1,\ldots,x_n]$}
\label{fig:mapAccuml}
\end{figure}

In our case, the `accumulator' is the heap, with initial value
@hInitial@.  The list $[x_1,\ldots,x_n]$ is the supercombinator
definitions, @sc_defs@, while the result list $[y_1, \ldots, y_n]$ is
the association of supercombinator names and addresses, @sc_addrs@.
Here, then, is the definition of @buildInitialHeap@.

> buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
> buildInitialHeap sc_defs
>  = (heap2, sc_addrs ++ prim_addrs)
>    where
>    (heap1, sc_addrs)   = mapAccuml allocateSc hInitial sc_defs
>    (heap2, prim_addrs) = mapAccuml allocatePrim heap1 primitives


\par
The `processing function', which we will call @allocateSC@,
allocates a single supercombinator, returning a new heap and a member
of the @sc_addrs@ association list.

> allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
> allocateSc heap (name, args, body)
>  = (heap', (name, addr))
>    where
>    (heap', addr) = hAlloc heap (NSupercomb name args body)

That completes the definition of the compiler. Next, we turn our attention
to the evaluator.

\subsection{The evaluator\index{evaluator}}

The evaluator @eval@ takes an initial machine state,
and runs the machine one step at a time, returning the list of
all states it has been through.

@eval@ always returns the current state as the first element of its
result.  If the current state is a final state, no further states are
returned; otherwise, @eval@ is applied recursively to the next state.
The latter is obtained by
taking a single step (using @step@), and then calling @doAdmin@ to
do any administrative work required between steps.

> eval state = state : rest_states
>              where
>              rest_states | tiFinal state = []
>                          | otherwise = eval next_state
>              next_state  = doAdmin (step state)

> doAdmin :: TiState -> TiState
> doAdmin = gc . (applyToStats tiStatIncSteps)

\subsubsection{Testing for a final state}

The function @tiFinal@ detects the final state\index{final state}.  We
are only finished if the dump is empty, and the stack contains a
single object, being either a number or a data object.

> tiFinal :: TiState -> Bool
>
> tiFinal ([sole_addr], [], heap, globals, stats)
>  = isDataNode (hLookup heap sole_addr)
>
> tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
> tiFinal state = False

Notice that the stack element is an address, which we need to look up in
the heap before we can check whether it is a number or not.
We should also produce a sensible error message if the stack should be empty
(which should never happen).

Finally, we can define @isDataNode@:

> isDataNode :: Node -> Bool
> isDataNode (NNum _) = True
> isDataNode (NData _ _) = True
> isDataNode _     = False

\subsubsection{Taking a step}

The function @step@ maps one state into its successor:

> step :: TiState -> TiState

It has to do case analysis on the node on top
of the spine stack\index{spine stack}, so it extracts
this node from the heap,
and uses @dispatch@ to call an appropriate function to
do the hard work for each form of node.

> step state
>  = dispatch (hLookup heap (hd stack))
>    where
>    (stack, dump, heap, globals, stats) = state
>
>    dispatch (NNum n)                  = numStep state n
>    dispatch (NAp a1 a2)               = apStep  state a1 a2
>    dispatch (NSupercomb sc args body) = scStep  state sc args body
>    dispatch (NInd addr)               = indStep state addr
>    dispatch (NPrim _ p)               = primStep state p
>    dispatch (NData t as)              = dataStep state

The definition of @numStep@ must be changed
to implement \ruleref{rule:num-return}.
If the stack contains
just one item, the address of an @NNum node@, and the dump is non-empty,
@numStep@ should pop the top element of the dump and make it into the new stack.
If these conditions do not apply, it should signal an error.

> numStep :: TiState -> Int -> TiState
> numStep ([_], (s:dump), heap, globals, stats) n = (s, dump, heap, globals, stats)
> numStep _ _ = error "Number applied as a function!"

Dealing with an application node is described by the unwind\index{unwind}
rule
(\ruleref{rule:unwind}),

> apStep :: TiState -> Addr -> Addr -> TiState
> apStep (stack, dump, heap, globals, stats) a1 a2
>  = case a2' of
>             NInd a3 -> (stack, dump, hUpdate heap (head stack) (NAp a1 a3), globals, stats)
>             _ -> (a1 : stack, dump, heap, globals, stats)
>    where a2' = hLookup heap a2

> indStep ((_:stack), dump, heap, globals, stats) a
>  = (a : stack, dump, heap, globals, stats)

\subsubsection{Applying a supercombinator}
\label{instantiate}

To apply a supercombinator, we must
instantiate its body,
binding the argument names to the argument addresses found
in the stack (\Ruleref{rule:sc1}).
Then we discard the arguments from the stack, including
the root of the redex, and push the (root of the) result of the
reduction onto the stack instead.

Instead of calling instantiate, we call instantiateAndUpdate, which
will inject indirection if necessary.

> scStep   :: TiState -> Name -> [Name] -> CoreExpr -> TiState
> scStep (stack, dump, heap, globals, stats) sc_name arg_names body
>  = (new_stack, dump, new_heap, globals, stats)
>    where
>    new_stack = if length stack < n+1 then error $ sc_name ++ " is applied to too few arguments."
>                else drop n stack
>                    where n = length arg_names
>    new_heap = instantiateAndUpdate body (head new_stack) heap env
>    env = arg_bindings ++ globals
>    arg_bindings = zip arg_names (getargs heap stack)

\par
In order to apply supercombinators and primitives, we need an
auxiliary function.
The function @getArgs@ takes a stack (which must consist
of a supercombinator on top of a stack of application nodes),
and returns a list formed from the argument of each of the application nodes
on the stack.

> -- now getargs since getArgs conflicts with Gofer standard.prelude
> getargs :: TiHeap -> TiStack -> [Addr]
> getargs heap (sc:stack)
>  = map get_arg stack
>    where get_arg addr = arg  where (NAp fun arg) = hLookup heap addr

\par
The @instantiate@ function takes an expression, a heap and
an environment associating
names with addresses.\index{instantiation}\index{template instantiation}
It creates an instance of the expression in the heap, and
returns the new heap and address of the root of the instance.
The environment is used by @instantiate@
to specify the addresses to be substituted for supercombinators and
local variables.

> instantiate :: CoreExpr              -- Body of supercombinator
>                -> TiHeap             -- Heap before instantiation
>                -> ASSOC Name Addr    -- Association of names to addresses
>                -> (TiHeap, Addr)     -- Heap after instantiation, and
>                                      -- address of root of instance

\par
\label{page:instantiate}
The case for numbers is quite straightforward.

> instantiate (ENum n) heap env = hAlloc heap (NNum n)

\par
The case for applications is also simple; just instantiate the
two branches, and build the application node.
Notice how we `thread' the heap though the recursive calls to @instantiate@.
That is, the first call to instantiate is given a heap and produces a new
heap; the latter is given to the second call to instantiate, which produces
yet another heap; the latter is the heap in which the new application node
is allocated, producing a final heap which is returned to the caller.

> instantiate (EAp e1 e2) heap env
>  = hAlloc heap2 (NAp a1 a2) where (heap1, a1) = instantiate e1 heap  env
>                                   (heap2, a2) = instantiate e2 heap1 env

\par
For variables, we simply look up the name in the environment
we are given, producing a suitable error message if we do not find
a binding for it.

> instantiate (EVar v) heap env
>  = (heap, aLookup env v (error ("Undefined name " ++ show v)))

@aLookup@, which is defined in Appendix~\ref{sect:assoc}, looks up
a variable in an association list, but returns its third argument if the
lookup fails.

> instantiate (EConstr tag arity) heap env
>            = hAlloc heap $ NPrim "Pack" $ PrimConstr tag arity

What you will need to do to instantiate \mbox{@(ELet nonRecursive defs body)@}
is:
\begin{enumerate}
\item
instantiate the
right-hand side of each of the definitions in @defs@;
\item
augment the
environment to bind the names in @defs@ to the addresses of the
newly constructed instances;
\item
call @instantiate@ passing the augmented
environment and the expression @body@.
\end{enumerate}

This still only takes care of @let@ expressions.
The result of instantiating a @letrec@ expression is a {\em cyclic\/} graph,
whereas @let@ expressions give rise to acyclic graphs.

For letrec: do everything exactly as in the @let@ case, except that in
Step 1 pass the {\em augmented\/} environment (constructed in
Step 2) to @instantiate@, instead of the {\em existing\/} environment.

> instantiate (ELet isrec defs body) heap env
>               = instantiate body heap' env'
>     where (heap', arg_bindings) = mapAccuml f heap defs
>           env' = arg_bindings ++ env
>           f :: TiHeap -> (Name, CoreExpr) -> (TiHeap, (Name, Addr))
>           f h (name, def) = (h', (name, addr))
>               where (h', addr) = instantiate def h env'

> instantiate (ECase e alts) heap env = error "Can't instantiate case exprs"

\subsection{Formatting the results}

So the @showResults@ function formats the output for us, using the
@iseq@ data type introduced in Section~\ref{sect:pretty}.

> showResults states
>  = iDisplay (iConcat [ iLayn (map showState states),
>                        showStats (last states)
>                      ])

We display the state just by showing the contents of the stack.  It is
too tiresome to print the heap in its entirety after each step, so
we will content ourselves with printing the contents of nodes referred to
directly from the stack.  The other components of the state do not change,
so we will not print them either.

> showState :: TiState -> Iseq
> showState (stack, dump, heap, globals, stats)
>  = iConcat [ showStack heap stack, iNewline, iStr "Heap size: ", iStr $ show $ hSize heap, iNewline ]
> {- show heap details:    
>              ,iConcat [
>               iStr "[",
>               iIndent (iInterleave iNewline (map show_stack_item $ hAddresses heap)),
>               iStr " ]"
>              ]
>            ]
>    where
>    show_stack_item addr
>     = iConcat [ showFWAddr addr, iStr ": ",
>                 showStkNode heap (hLookup heap addr)
>       ]-}

\par
We display the stack, topmost element first, by displaying the address on
the stack, and the contents of the node to which it points.  Most of these
nodes are application nodes, and for each of these we
also display the contents of its argument node.

> showStack :: TiHeap -> TiStack -> Iseq
> showStack heap stack
>  = iConcat [
>        iStr "Stk [",
>        iIndent (iInterleave iNewline (map show_stack_item stack)),
>        iStr " ]"
>    ]
>    where
>    show_stack_item addr
>     = iConcat [ showFWAddr addr, iStr ": ",
>                 showStkNode heap (hLookup heap addr)
>       ]

> showStkNode :: TiHeap -> Node -> Iseq
> showStkNode heap (NAp fun_addr arg_addr)
>  = iConcat [   iStr "NAp ", showFWAddr fun_addr,
>                iStr " (", showNode (hLookup heap fun_addr), iStr ")",
>                iStr " ", showFWAddr arg_addr, iStr " (",
>                showNode (hLookup heap arg_addr), iStr ")"
>    ]
> showStkNode heap (NInd addr)
>  = iConcat [   iStr "NInd", showFWAddr addr,
>                iStr " (", showNode (hLookup heap addr), iStr ")"
>    ]
> showStkNode heap node = showNode node

\par
@showNode@ displays the value of a @node@.
It prints only the name stored
inside @NSupercomb@ nodes, rather than printing the
complete value; indeed this is the only reason the name is stored inside
these nodes.

> showNode :: Node -> Iseq
> showNode (NAp a1 a2) = iConcat [ iStr "NAp ", showAddr a1,
>                                  iStr " ",    showAddr a2
>                        ]
> showNode (NSupercomb name args body) = iStr ("NSupercomb " ++ name)
> showNode (NNum n) = (iStr "NNum ") `iAppend` (iNum n)
> showNode (NInd a) = (iStr "NInd ") `iAppend` (showAddr a)
> showNode (NPrim _ (PrimConstr t n)) = iConcat $ map iStr ["Pack{", show t, ",", show n, "}"]
> showNode (NPrim n _) = iStr n
> showNode (NData t as) = (iConcat $ map iStr [ "NData ", show t])
>                         `iAppend` x
>                             where x = case as of
>                                       [] -> INil
>                                       _ -> iAppend (iStr " ") $ iStr $ show as

> showAddr :: Addr -> Iseq
> showAddr addr = iStr (show addr)
>
> showFWAddr :: Addr -> Iseq    -- Show address in field of width 4
> showFWAddr addr = iStr (space (4 -  length str) ++ str)
>                   where
>                   str = show addr

\par
@showStats@ is responsible for printing out the accumulated statistics:

> showStats :: TiState -> Iseq
> showStats (stack, dump, heap, globals, stats)
>  = iConcat [ iNewline, iNewline, iStr "Total number of steps = ",
>              iNum $ tiStatGetSteps stats
>    ]

> instantiateAndUpdate 
>     :: CoreExpr             -- Body of supercombinator
>        -> Addr              -- Address of node to update
>        -> TiHeap            -- Heap before instantiation
>        -> ASSOC Name Addr   -- Associate parameters to addresses
>        -> TiHeap            -- Heap after instantiation

> instantiateAndUpdate (ENum n) upd_addr heap _ = hUpdate heap upd_addr (NNum n)

> instantiateAndUpdate (EAp e1 e2) upd_addr heap env
>  = hUpdate heap2 upd_addr (NAp a1 a2)
>    where
>    (heap1, a1) = instantiate e1 heap  env
>    (heap2, a2) = instantiate e2 heap1 env

> instantiateAndUpdate (EVar v) upd_addr heap env
>  = hUpdate heap upd_addr $ NInd addr
>    where addr = aLookup env v (error ("Undefined name " ++ show v))

> instantiateAndUpdate (EConstr tag arity) upd_addr heap env
>            = hUpdate heap upd_addr $ NPrim "Pack" $ PrimConstr tag arity

> instantiateAndUpdate (ELet isrec defs body) upd_addr heap env
>               = instantiateAndUpdate body upd_addr heap' env'
>     where (heap', arg_bindings) = mapAccuml f heap defs
>           env' = arg_bindings ++ env
>           f :: TiHeap -> (Name, CoreExpr) -> (TiHeap, (Name, Addr))
>           f h (name, def) = (h', (name, addr))
>               where (h', addr) = instantiate def h env'

> instantiateAndUpdate (ECase e alts) upd_addr heap env = error "Can't instantiate case exprs"

\section{Mark 4: Adding arithmetic}

\subsection{Implementing arithmetic}

To implement arithmetic we need to make a number of changes.
First, we need to redefine the type @tiDump@ to be a stack of stacks, whose
initial value is empty.\index{dump!in template-instantiation machine}

> type TiDump = [TiStack]
> initialTiDump = []

> data Primitive = Neg | Add | Sub | Mul | Div -- arithmetics
>                | PrimConstr Int Int | If | Greater | GreaterEq | Less | LessEq | Eq | NotEq -- data structures

\par
We define an association
list giving the mapping from variable names to primitives, thus:

> primitives :: ASSOC Name Primitive
> primitives = [ ("negate", Neg),
>                ("+", Add),   ("-", Sub),
>                ("*", Mul),   ("/", Div),
>                ("if", If),
>                (">", Greater), (">=", GreaterEq),
>                ("<", Less), ("<=", LessEq),
>                ("==", Eq), ("~=", NotEq)
>              ]

To add further primitives, just add more constructors to the
@primitive@ type, and more elements to the @primitives@ association list.

We can then define @allocatePrim@, very much as we defined @allocateSc@:

> allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
> allocatePrim heap (name, prim)
>  = (heap', (name, addr))
>    where
>    (heap', addr) = hAlloc heap (NPrim name prim)

> primStep state Neg = primNeg state
> primStep state Add = primArith state (+)
> primStep state Sub = primArith state (-)
> primStep state Mul = primArith state (*)
> primStep state Div = primArith state (div)
> primStep state (PrimConstr t n) = primConstr state t n
> primStep state If = primIf state
> primStep state Greater = primCompare state (>)
> primStep state GreaterEq = primCompare state (>=)
> primStep state Less = primCompare state (<)
> primStep state LessEq = primCompare state (<=)
> primStep state Eq = primCompare state (==)
> primStep state NotEq = primCompare state (/=)

@primNeg@ needs to do the following:
\begin{itemize}
\item
Use @getArgs@ to extract
the address of the argument from the stack, and @hLookup@ to get the node
pointed to by this address.
\item
Use the auxiliary function @isDataNode@ to
check if the argument node is evaluated.

\item
If it is not evaluated, use \ruleref{rule:negate-eval1} to set up the new
state ready to evaluate the argument.
This involves pushing the current stack on the dump, and making a new stack
whose only element is the argument to @negate@.

\item
If it is evaluated, use @hUpdate@ to overwrite
the root of the redex with an @NNum@ node
containing the result, and return, having modified the stack appropriately.
\end{itemize}

> primNeg :: TiState -> TiState
> -- The spine stack must only contain the Neg node and the Ap node
> primNeg (stack, dump, heap, globals, stats)
>     | isDataNode arg = (stack', dump, h', globals, stats)
>     | otherwise = ([b], stack':dump, heap, globals, stats)
>                    where
>                    b = head $ getargs heap stack
>                    arg = hLookup heap b 
>                    stack' = tail stack
>                    h' = hUpdate heap (stack !! 1) (NNum (-n))
>                        where (NNum n) = arg

> primArith :: TiState -> (Int -> Int -> Int) -> TiState
> primArith state op = primDyadic state op'
>     where op' (NNum n1) (NNum n2) = NNum $ op n1 n2

> primDyadic :: TiState -> (Node -> Node -> Node) -> TiState
> primDyadic (stack, dump, heap, globals, stats) op
>     | not $ isDataNode arg1 = ([b1], (tail stack):dump, heap, globals, stats)
>     | not $ isDataNode arg2 = ([b2], stack':dump, heap, globals, stats)
>     | otherwise = (stack', dump, h', globals, stats)
>                    where
>                    (b1:b2:_) = getargs heap stack
>                    arg1 = hLookup heap b1
>                    arg2 = hLookup heap b2
>                    (_:_:stack') = stack
>                    h' = hUpdate heap (stack !! 2) $ op arg1 arg2


\section{Mark 5: Structured data}

It would be nice to give an implementation for the @case@ expressions
of our core language, but it turns out that it is rather hard to do
so within the framework of a template instantiation machine.
(Our later implementations will not have this problem.)
Instead we will use a collection of built-in
functions, such as @if@, @casePair@ and
@caseList@, which allow us to manipulate certain structured types.  The
template machine will remain unable to handle general structured objects.

This is getting so boring so I'm not implementing pairs and lists.

Section 2.8.3 in the book outlines another interesting approach to
structured data: Church encoding. This representation gets rid of the
inconvenient primitives by encoding everything as a function.

> primConstr (stack, dump, heap, globals, stats) t n
>  = (new_stack, dump, new_heap, globals, stats)
>    where
>    new_stack = if length stack < n+1 then error $ "Pack{" ++ show t ++ "," ++ show n ++ "} is applied to too few arguments."
>                else drop n stack
>    new_heap = hUpdate heap (head new_stack) (NData t components)
>    components = take n (getargs heap stack)

> dataStep ([_], (s:dump), heap, globals, stats) = (s, dump, heap, globals, stats)
> dataStep _ = error "Data applied as a function!"

> primIf (stack, dump, heap, globals, stats)
>  = state' 
>    where
>    stack' = if length stack < 4 then error "if expression incomplete."
>             else drop 3 stack
>    [b,t,f] = take 3 (getargs heap stack)
>    bVal = hLookup heap b
>    heap' = hUpdate heap (head stack') $ case bVal of
>                                                   NData 1 _ -> hLookup heap f
>                                                   NData 2 _ -> hLookup heap t
>                                                   _ -> error "boolean expression ill defined."
>    state' = if isDataNode bVal then (stack', dump, heap', globals, stats)
>             else ([b], (tail stack):dump, heap, globals, stats)

> primCompare :: TiState -> (Int -> Int -> Bool) -> TiState
> primCompare state@(_,_,heap,globals,_) op = primDyadic state op'
>     where op' (NNum n1) (NNum n2) = if op n1 n2 then hLookup heap $ aLookup globals "True" $ error "True undefined!?"
>                                     else hLookup heap $ aLookup globals "False" $ error "False undefined!?"

\section{Garbage collection\index{garbage collection}\advanced}

> gc :: TiState -> TiState
> gc s@(stack, dump, heap, globals, stats) = -- if hSize heap > 100 then s else
>    (s', d', h', g', stats)
>     where (h1, s') = markFromStack heap stack
>           (h2, d') = markFromDump h1 dump
>           (h3, g') = markFromGlobals h2 globals
>           h' = scanHeap h3

whose result state behaves exactly like its input state, except that
it has a (hopefully) smaller heap.  This smaller heap contains all the
nodes which are accessible from the other components of the machine
state, directly or indirectly.  @gc@ makes the heap smaller by
calling @hFree@ on the addresses of nodes which are no longer required.

\subsection{Mark-scan collection}\index{mark-scan collection}

To begin with, we will develop a {\em mark-scan collector}.
This works in three phases:
\begin{enumerate}
\item
The first phase identifies all the {\em roots\/}; that is,
all the heap addresses contained in the machine state.  Where can such addresses
be lurking?  We can easily find out by looking at the types involved in
the machine state for occurrences of @addr@.  The answer is that addresses
can occur in the stack, the dump and the globals.  So we need the following
functions:

> markFromStack   :: TiHeap -> TiStack   -> (TiHeap,TiStack)
> markFromStack = mapAccuml markFrom  

> markFromDump    :: TiHeap -> TiDump    -> (TiHeap,TiDump)
> markFromDump = mapAccuml markFromStack

> markFromGlobals :: TiHeap -> TiGlobals -> (TiHeap,TiGlobals)
> markFromGlobals h g = let (n, a) = unzip g
>                           (h', x) = mapAccuml markFrom h a
>                       in (h', zip n x)

\item
In the {\em mark phase}, each node whose address is in the machine
state is {\em marked}.  When a node is marked, all its descendants
are also marked, and so on recursively.
The @markFrom@ function takes a heap and an address, and returns a new
heap in which all the nodes accessible from the address have been marked,
together with a new address
which should be used instead of the old one}.

> markFrom :: TiHeap -> Addr -> (TiHeap, Addr)

\item
It uses @hAddresses@ to extract the list of
all the addresses used in the heap, and examines each in turn.  If the node
to which it refers is unmarked (that is, not an @NMarked@ node), it calls
@hFree@ to free the node.  Otherwise, it unmarks the node by
using @hUpdate@ to replace it with the node found inside the
@NMarked@ constructor.

> scanHeap :: TiHeap -> TiHeap
> scanHeap h = let as = hAddresses h
>                  f addr heap = let value = hLookup heap addr in
>                                case value of
>                                  (NMarked x) -> hUpdate heap addr x
>                                  _ -> hFree heap addr
>              in foldr f h as 

Now we are ready to define @markFrom@.
Given an address $a$ and a heap $h$, it does the following:
\begin{enumerate}
\item
It looks up $a$ in $h$, giving a node $n$.  If it is already marked, @markFrom@
returns immediately.  This is what prevents the marking process from going
on forever when it encounters a cyclic structure in the heap.
\item
It marks the node by using @hUpdate@ to replace it with $@NMarked@~n$.
\item
It extracts any addresses from inside $n$ (there may be zero or more
such addresses), and calls @markFrom@ on each of them.
\end{enumerate}

> markFrom h a = case hLookup h a of
>                  NMarked _ -> (h, a)
>                  NInd x -> markFrom h x
>                  NAp a1 a2 -> let (h1, a1') = markFrom h a1
>                                   (h2, a2') = markFrom h1 a2
>                                   x = NMarked $ NAp a1' a2'
>                               in (hUpdate h2 a x, a)
>                  NData x as -> let (h', as') = mapAccuml markFrom h as
>                                    y = NMarked $ NData x as'
>                                in (hUpdate h' a y, a)
>                  x -> (hUpdate h a $ NMarked x, a)

The markFrom function is recursively defined. The book described the
"pointer reversal" approach that uses two pointers to traverse
structured data without recursion. There's more information at
http://en.wikipedia.org/wiki/Garbage_collection_(computer_science).

The book also described a copying collector by Cheney at
http://en.wikipedia.org/wiki/Cheney's_algorithm. When copying a node,
this algorithm doesn't recursively copies nodes pointed to by the
current node. The first stage only copies root nodes. The second stage
scans the to-space linearly, with two "fingers". One finger points to
the beginning of the to-space, and the other finger points to the
beginning of the free space. The linear scanning stops when the
scanning finger catches up with the allocation finger.
