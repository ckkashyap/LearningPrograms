> module Gm where
> import Language
> import Utils

The `Big Idea' of the G-machine\index{G-machine!the Big Idea}, and other
compiled implementations, is this:
\begin{important}
Before running the program, translate each supercombinator body to a
sequence of instructions which, when executed, will construct an
instance of the supercombinator body.
\end{important}

Executing this code should be faster than calling an instantiation
function, because all the instructions are concerned with constructing
the instance.  There are no instructions required to traverse the
template, because all that has been done during the translation
process. Running a program is thereby split into two stages. In the
first stage a compiler is used to produce some intermediate form of the
program; this is referred to as {\em compile-time}\index{compile-time}.
 In the second stage the intermediate form is executed; this is called
{\em run-time}\index{run-time}.

At the top level the G-machine is very similar to the template instantiator.

> runProg :: FilePath -> IO String
> runProg f = readFile f >>= (return . showResults . eval . compile . parseCore)

\subsection{Data type definitions}

In describing the G-machine, we will make use of state {\em access
functions\/}\index{access functions} to access the components of a
state. The advantage of this approach is that when we modify the state
to accommodate new components, we may reuse most of the original code
we have written. We will use the prefix @get@ to denote an access
function that gets a component from a state, and the prefix @put@ to
replace a component in a state.

We consider the type definitions of each of the five components of the
state, and their access functions, in turn.

\begin{itemize}
\item The instruction stream is of type @gmCode@ and is simply a list
of @instruction@s.

> type GmCode = [Instruction]

To get convenient access to the code, when the state is later
augmented with extra components, we define two functions: @getCode@ and
@putCode@.

> getCode :: GmState -> GmCode
> getCode (_, i, stack, dump, heap, globals, stats) = i

> putCode :: GmCode -> GmState -> GmState
> putCode i' (o, _, stack, dump, heap, globals, stats)
>    = (o, i', stack, dump, heap, globals, stats)

\item The G-machine stack @gmStack@ is a list of addresses in the heap.

> type GmStack = [Addr]

To get convenient access to the stack, when the state is later
augmented with extra components, we define two functions @getStack@ and
@putStack@

> getStack :: GmState -> GmStack
> getStack (_, i, stack, dump, heap, globals, stats) = stack

> putStack :: GmStack -> GmState -> GmState
> putStack stack' (o, i, stack, dump, heap, globals, stats)
>    = (o, i, stack', dump, heap, globals, stats)

\item Just as we did in the case of the template instantiator, we use
the heap data structure from @utils@ to implement heaps.

> type GmHeap = Heap Node

Again, to access this component of the state we define access
functions.

> getHeap :: GmState -> GmHeap
> getHeap (_, i, stack, dump, heap, globals, stats) = heap

> putHeap :: GmHeap -> GmState -> GmState
> putHeap heap' (o, i, stack, dump, heap, globals, stats)
>    = (o, i, stack, dump, heap', globals, stats)

Number nodes contain the relevant number; application nodes apply the
function at the first address to the expression at the second address.
The @NGlobal@ node contains the number of arguments that the global
expects and the code sequence to be executed when the global has
enough arguments. This replaces the @NSupercomb@ nodes of the template
instantiator, which held a template instead of the arity and code.

\item Because we will later be making a lazy implementation it is
important that there is only one node for each global. The address of
a global can be determined by looking up its value in the association
list @gmGlobals@. This corresponds to the @tiGlobals@ component
of the template machine.

> type GmGlobals = ASSOC Name Addr

The access function we use is @getGlobals@; in the Mark~1 machine, this
component is constant so we do not need a corresponding put function.

> getGlobals :: GmState -> GmGlobals
> getGlobals (_, i, stack, dump, heap, globals, stats) = globals

\item The statistics component of the state is implemented as an
abstract data type.

> statInitial  :: GmStats
> statIncSteps :: GmStats -> GmStats
> statGetSteps :: GmStats -> Int

> type GmStats = Int
> statInitial    = 0
> statIncSteps s = s+1
> statGetSteps s = s

To access this component we define @getStats@ and @putStats@:

> getStats :: GmState -> GmStats
> getStats (_, i, stack, dump, heap, globals, stats) = stats

> putStats :: GmStats -> GmState -> GmState
> putStats stats' (o, i, stack, dump, heap, globals, stats)
>    = (o, i, stack, dump, heap, globals, stats')

\end{itemize}

\subsection{The evaluator\index{G-machine!evaluator}}
\label{gm:ss:eval1}

The G-machine evaluator, @eval@, is defined to produce a list of
states.  The first one is the one constructed by the compiler. If
there is a last state, then the result of the evaluation will be on
the top of the stack component of the last state.

> eval :: GmState -> [GmState]
> eval state = state: restStates
>              where
>              restStates | gmFinal state     = []
>                         | otherwise         = eval nextState
>              nextState  = doAdmin (step state)

The function @doAdmin@ uses @statIncSteps@ to modify the statistics
component of the state.

> doAdmin :: GmState -> GmState
> doAdmin s = putStats (statIncSteps (getStats s)) s

The important parts of the evaluator are the functions @gmFinal@ and
@step@ which we will now look at.

\subsubsection{Testing for a final state\index{termination
condition!in G-machine}}

The G-machine interpreter has finished when the code sequence that it
is executing is empty. We express this condition in the @gmFinal@
function.

> gmFinal :: GmState -> Bool
> gmFinal s = case (getCode s) of
>                    []        -> True
>                    _ -> False

\subsubsection{Taking a step}

The @step@ function is defined so that it makes a state transition
based on the instruction it is executing.

> step :: GmState -> GmState
> step state = dispatch i (putCode is state)
>              where (i:is) = getCode state

\par
We @dispatch@ on the current instruction @i@ and replace the current
code sequence with the code sequence @is@; this corresponds to
advancing the program counter in a real machine.

> dispatch :: Instruction -> GmState -> GmState
> dispatch (Pushglobal f) = pushglobal f
> dispatch (Pushint n)    = pushint n
> dispatch Mkap           = mkap
> dispatch (Push n)       = push n
> dispatch (Update n)     = update n
> dispatch (Pop n)        = pop n
> dispatch Unwind         = unwind
> dispatch (Slide n)      = slide n
> dispatch (Alloc n)      = alloc n
> dispatch Eval           = ev
> dispatch Add            = arithmetic2 (+)
> dispatch Sub            = arithmetic2 (-)
> dispatch Mul            = arithmetic2 (*)
> dispatch Div            = arithmetic2 div
> dispatch Neg            = arithmetic1 negate
> dispatch Eq             = comparison (==)
> dispatch Ne             = comparison (/=)
> dispatch Lt             = comparison (<)
> dispatch Le             = comparison (<=)
> dispatch Gt             = comparison (>)
> dispatch Ge             = comparison (>=)
> dispatch (Pack t n)     = pack t n
> dispatch (Casejump brs) = cjmp brs
> dispatch (Split n)      = split n
> dispatch Print          = prn

Let us begin by looking at the transition rules for the postfix
instructions. There will be one for each syntactic object in
@instruction@. We begin with the @Pushglobal@ instruction, which uses the
@globals@ component of the state to find the unique @NGlobal@ node in the
@heap@ that holds the global $f$. If it cannot find one, it prints a
suitable error message.

\gmrule%
{\gmstate{@Pushglobal@\ f:i}{s}{h}{m[f:a]}}%
{\gmstate{i}{a:s}{h}{m}}

We implement this rule using the @pushglobal@ function.

> pushglobal :: Name -> GmState -> GmState
> pushglobal f state
>       = putStack (a: getStack state) state
>       where a = aLookup (getGlobals state) f (error ("Undeclared global " ++ f))

\par
The remaining transitions are for constructing the body of a
supercombinator. The transition for @Pushint@ places an integer node
into the heap.

\gmrule%
{\gmstate{@Pushint@\ n:i}{s}{h}{m}}%
{\gmstate{i}{a:s}{h[a:@NNum@\ n]}{m}}

The corresponding function is @pushint@.
The number is placed in the new heap @heap'@ with address @a@. We then
place the heap and stack back into the state.

> pushint :: Int -> GmState -> GmState
> pushint n state
>       = putHeap heap' (putStack (a: getStack state) state)
>       where (heap', a) = hAlloc (getHeap state) (NNum n)

\par
The @Mkap@ instruction uses the two addresses on the top of the stack
to construct an application node in the heap. It has the following
transition rule.

\gmrule%
{\gmstate{@Mkap@:i}{a_1:a_2:s}{h}{m}}%
{\gmstate{i}{a:s}{h[a: @NAp@\ a_1\  a_2]}{m}}

This transition becomes @mkap@. Again @heap'@ and @a@ are respectively
the new heap and the address of the new node.

> mkap :: GmState -> GmState
> mkap state
>       = putHeap heap' (putStack (a:as') state)
>       where (heap', a)  = hAlloc (getHeap state) (NAp a1 a2)
>             (a1:a2:as') = getStack state

\par
The @Push@ instruction is used to take a copy of an argument which was
passed to a function.

> push :: Int -> GmState -> GmState
> push n state
>    = putStack (a:as) state
>    where   as = getStack state
>            a  = as !! n

This uses the auxiliary function @getArg@ to select the required
expression from an application node.

> getArg :: Node -> Addr
> getArg (NAp a1 a2) = a2

> update n state = putHeap h' $ putStack as state
>     where (a:as) = getStack state
>           h = getHeap state
>           h' = hUpdate h (as !! n) $ NInd a

> slide :: Int -> GmState -> GmState
> slide n state
>       = putStack (a: drop n as) state
>       where (a:as) = getStack state

> pop n state = putStack (drop n $ getStack state) state

> alloc n state = putStack s' $ putHeap h' state
>     where s = getStack state
>           h = getHeap state
>           (h', as) = allocNodes n h
>           s' = as ++ s

> allocNodes :: Int -> GmHeap -> (GmHeap, [Addr])
> allocNodes 0     heap = (heap,  [])
> allocNodes n heap = (heap2, a:as)
>                         where (heap1, as) = allocNodes (n-1) heap
>                               (heap2, a)  = hAlloc heap1 (NInd hNull)

\par
@Unwind@ is the most complex instruction because it replaces the outer
loop of our template instantiator. The @Unwind@ instruction is always
the last instruction of a sequence, as we shall see in the next
section. The @newState@ constructed depends on the item on top of the
stack; this depends on the transition rule that is fired, which also
depends on the item on top of the stack.

> unwind :: GmState -> GmState
> unwind state
>      = newState (hLookup heap a)
>      where
>              stack@(a:as) = getStack state
>              heap   = getHeap state

\par
We first consider the case where there is a number on top of the
stack. 

>              -- We assume that the dump is never empty when we unwind a WHNF.
>              -- This is guaranteed by changing the initial code from Unwind to Eval.
>              newState (NNum _) = putDump d $ putCode i $ putStack (a:s) state
>                  where (i,s):d = getDump state

>              newState (NConstr _ _) = putDump d $ putCode i $ putStack (a:s) state
>                  where (i,s):d = getDump state

\par
If there is an application node on top of the stack then we must
continue to unwind from the next node.

\gmrule%
{\gmstate{[@Unwind@]}{a:s}{h[a: @NAp@\  a_1\  a_2]}{m}}%
{\gmstate{[@Unwind@]}{a_1:a:s}{h}{m}}

>              newState (NAp a1 a2)   = putCode [Unwind] (putStack (a1:a:as) state)

\par
The most complicated rule occurs when there is a global node on top of
the stack. There are two cases to consider, depending on whether there are
enough arguments to reduce the supercombinator application.

Firstly, if there are not enough arguments to reduce the
supercombinator application then the program was ill-typed. We will
ignore this case for the Mark~1 G-machine. Alternatively, when there
are enough arguments, it is possible to reduce the supercombinator, by
`jumping to' the code for the supercombinator. In the transition
rule this is expressed by moving the supercombinator code into the
code component of the machine.

>              newState (NGlobal n c) 
>                     | length as < n        = error "Unwinding with too few arguments"
>                     | otherwise    = putCode c state'
>                     where state' = putStack (rearrange n heap stack) state

>              newState (NInd a') = putCode [Unwind] $ putStack (a':as) state

> rearrange :: Int -> GmHeap -> GmStack -> GmStack
> rearrange n heap as
>       = take n as' ++ drop n as
>       where as' = map (getArg . hLookup heap) (tl as)

\subsection{Compiling a program\index{G-machine!compiler}}
\label{gm:ss:compile}

The @compile@ function turns a program into an initial state for the
G-machine.  The initial code sequence finds the global @main@ and then
evaluates it. The heap is initialised so that it contains a node for
each global declared. @globals@ contains the map from global names to the
@NGlobal@ nodes provided for them.

> compile :: CoreProgram -> GmState
> compile program
>    = ([], initialCode, [], [], heap, globals, statInitial)
>    where (heap, globals) = buildInitialHeap program

\par
To construct the initial heap and to provide the map of the global
nodes for each global defined we use @buildInitialHeap@. This is just
as it was in the template machine.

> buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
> buildInitialHeap program
>    = mapAccuml allocateSc hInitial compiled
>      where compiled = map compileSc (program ++ preludeDefs ++ parseCore extraDefs) ++ compiledPrimitives

Compared with the hardcoded "if" from Mark 5, we have a redundant Eval instruction for each branch.
But that's OK because our new definition supports bools as a separate type from ints.
Eval is redundant here because "if" has a lazy version (here) and a strict version (below).

We could create strict versions for fst and snd as well.

>                where extraDefs = "if c t f = case c of <1> -> f; <2> -> t;\
>                                  \nil = Pack{1,0};\
>                                  \cons x xs = Pack{2,2} x xs;\
>                                  \MkPair a b = Pack{1,2} a b;\
>                                  \fst x = case x of <1> a b -> a;\
>                                  \snd x = case x of <1> a b -> b"


The @buildInitialHeap@ function uses @mapAccuml@ to allocate nodes for
each compiled global; the compilation occurring (where necessary) in
@compiled@, which has type @[gmCompiledSC]@.

> type GmCompiledSC = (Name, Int, GmCode)

The function @allocateSc@ allocates a new global for its compiled
supercombinator argument, returning the new heap and the address where
the global is stored.

> allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
> allocateSc heap (name, nargs, instns)
>       = (heap', (name, addr))
>       where (heap', addr) = hAlloc heap (NGlobal nargs instns)

\par
In the initial state, we want the machine to evaluate the value of the
program. We recall that this is just the value of the global @main@.

> initialCode :: GmCode
> initialCode = [Pushglobal "main", Eval, Print]

\par
Each supercombinator is compiled using @compileSc@, which implements
the \tSC{} scheme of Figure~\ref{gm:fg:schemes1}. It returns a triple
containing the supercombinator name, the number of arguments the
supercombinator needs before it can be reduced, and the code sequence
associated with the supercombinator.

> compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
> compileSc (name, env, body)
>       = (name, length env, compileR body (zip env [0..]))

This in turn uses @compileR@, which corresponds to the \tR{} scheme of
Figure~\ref{gm:fg:schemes1}.

> compileR :: GmCompiler
> compileR e env = x' ++ y
>     where n = length env
>           y | n == 0 = [Update 0, Unwind]
>             | otherwise = [Update n, Pop n, Unwind]
>           x = compileE e env
 
The last instruction is redundant if it's Eval
The book doesn't have this optimization, and so Eval may be forced on a partial application.
The new rule (3.29) for Unwind and Exercise (3.29) address this problem.
A failing example would be "J = I; main = J 0".
However, I cannot think of a counterexample with this optimization, so we don't need rule (3.29)?
          
>           x' = case (last x) of
>                  Eval -> init x
>                  _ -> x

Each of the compiler schemes has the same type: @gmCompiler@.

> type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

> type GmEnvironment = ASSOC Name Int

This constructs the instantiation of the supercombinator body using
@compileC@, which corresponds to the \tC{} scheme
of Figure~\ref{gm:fg:schemes1}.

> compileC :: GmCompiler
> compileC (EVar v)    env 
>  | elem v (aDomain env)          = [Push n]
>  | otherwise                     = [Pushglobal v]
>  where n = aLookup env v (error "Can't happen")
> compileC (ENum n)    env = [Pushint n]
> compileC e@(EAp e1 e2) env =
>     let findFunction (EAp x _) = findFunction x
>         findFunction x = x
>         c (EAp a b) p = compileC b p ++ c a (argOffset 1 p)
>         c (EConstr t a) _ = [Pack t a]
>     in case findFunction e of
>          EConstr t a -> c e env
>          _ -> compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]
> compileC (EConstr t a) _ = [Pack t a]
> compileC (ELet recursive defs e) args 
>          | recursive     = compileLetrec compileC defs e args
>          | otherwise     = compileLet    compileC defs e args

We can change the stack offsets using the function @argOffset@. 
If @env@ implements
$\rho$, then @(argOffset n env)@ implements $\rho^{+n}$.

> argOffset :: Int -> GmEnvironment -> GmEnvironment
> argOffset n env = [(v, n+m) | (v,m) <- env]

\subsection{Printing the results\index{execution traces!in G-machine}}

The printing is controlled by @showResults@. It produces three pieces
of output: the super-combinator code sequences, the state
transitions and the final statistics.

> showResults :: [GmState] -> [Char]
> showResults states
>       = iDisplay (iConcat [
>       iStr "Supercombinator definitions", iNewline,
>       iInterleave iNewline (map (showSC s) (getGlobals s)),
>       iNewline, iNewline, iStr "State transitions", iNewline, iNewline,
>       iLayn (map showState states),
>       iNewline, iNewline,
>       showStats (last states)])
>       where (s:ss) = states

\par
Taking each of these in turn, we begin with @showSC@. This finds the
code for the supercombinator in the unique global heap node
associated with the global, and prints the code sequence using
@showInstructions@.

> showSC :: GmState -> (Name, Addr) -> Iseq
> showSC s (name, addr)
>       = iConcat [ iStr "Code for ", iStr name, iNewline,
>             showInstructions code, iNewline, iNewline]
>       where (NGlobal arity code) = (hLookup (getHeap s) addr)

Then @showInstructions@ is used to output a code sequence.

> showInstructions :: GmCode -> Iseq
> showInstructions is
>       = iConcat [iStr "  Code:{",
>            iIndent (iInterleave iNewline (map showInstruction is)),
>            iStr "}", iNewline]

The output for each individual instruction is given by
@showInstruction@.

> showInstruction :: Instruction -> Iseq
> showInstruction (Casejump brs) = iConcat [iStr "Casejump [",
>                                           iIndent $ iInterleave iNewline (map f brs),
>                                           iStr "]"]
>     where f (l, is) = iConcat [iNum l, iStr " -> ", iStr "{",
>                               iIndent (iInterleave iNewline (map showInstruction is)),
>                               iStr "}"]
> showInstruction x = iStr $ show x

To correspond with our diagrams, we would like to have the top of
stack at the bottom of the printed stack. To this end we reverse the
stack.

> showStack :: GmState -> Iseq
> showStack s
>       = iConcat [iStr " Stack:[",
>            iIndent (iInterleave iNewline
>                        (map (showStackItem s) (reverse (getStack s)))),
>            iStr "]"]

\par
Each stack item is displayed using @showStackItem@. It prints the
address stored in the stack and the object in the heap to which it
points.

> showStackItem :: GmState -> Addr -> Iseq
> showStackItem s a
>       = iConcat [iStr (showaddr a), iStr ": ",
>            showNode s a (hLookup (getHeap s) a)]

\par
The function @showNode@ needs to invert the association list of global
names and heap addresses to display the global nodes it comes across.

> showNode :: GmState -> Addr -> Node -> Iseq
> showNode s a (NNum n)      = iNum n
> showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
>    where v = head [n | (n,b) <- getGlobals s, a==b]
> showNode s a (NAp a1 a2)   = iConcat [iStr "Ap ", iStr (showaddr a1),
>                                       -- possibly infinite loops!
> --                                      iStr " (", showNode s a1 (hLookup heap a1), iStr ")",
>                                       iStr " ",   iStr (showaddr a2)]
> --                                      iStr " (", showNode s a2 (hLookup heap a2), iStr ")"]
>                              where heap = getHeap s
> showNode s a (NInd 0)   = iStr "?"  -- A placeholder that will be updated later
> showNode s a (NInd a')   = iConcat [iStr "Ind ", iStr (showaddr a'),
>                                       iStr " (", showNode s a' (hLookup heap a'), iStr ")"]
>                              where heap = getHeap s
> showNode s a (NConstr t as)
>  = iConcat [iStr "Constr ", iNum t, iStr " [",
>             iInterleave (iStr ", ") (map (iStr.showaddr) as), iStr "]"]

\par
Finally, we print the accumulated statistics, using @showStats@.

> showStats :: GmState -> Iseq
> showStats s
>       = iConcat [ iStr "Steps taken = ", iNum (statGetSteps (getStats s))]


\section{Mark 2: Making it lazy}
\index{G-machine!Mark 2}\index{G-machine!laziness}\index{updates!in G-machine}
\label{gm:sc:mark2}

The Mark~1 machine is not lazy at the moment because it does not
overwrite the root node of the original expression before
unwinding. We do NOT mean that Mark~1 is strict: its evaluation order
is still normal order.

In the Mark~2 machine, the idea is that after instantiating the body
of the supercombinator, we will {\em overwrite\/}\index{overwrite} the
root of the original redex\index{redex} with an {\em indirection
node\/}\index{indirections} pointing to the newly constructed
instance. The effect is that the machine `remembers' the value that
was instantiated last time the redex was reduced, and hence does not
need to recalculate it.

We implement this change as follows. In the Mark~1 machine the code
for each supercombinator concluded with $[@Slide@\ (n+1),\ @Unwind@]$.
To capture updating we replace this with $[@Update@\ n,\ @Pop@\ n,\
@Unwind@]$. This is illustrated in the following diagrams, in which we
use @#@ to represent indirection nodes.

In place of the single instruction $@Slide@~n+1$ that we generated last
time we now generate the sequence of instructions $[@Update@\ n, \
@Pop@\ n]$. Therefore we are going to have to include these
instructions in the new instruction set.

To implement the indirection nodes we must have a new node type in the
heap: @NInd@ which we use for indirections.

\section{Mark 3: @let(rec)@ expressions\index{G-machine!Mark 3}}

It takes as arguments: the compilation
scheme @comp@ for the body @e@, the definitions @defs@ and the current
environment @env@.

> compileLet :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
> compileLet comp defs expr env
>   = compileLet' defs env ++ comp expr env' ++ [Slide (length defs)]
>     where env' = compileArgs defs env

\par
The compilation of the new definitions is accomplished by the function
@compileLet'@.

> compileLet' :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
> compileLet' []                  env = []
> compileLet' ((name, expr):defs) env
>     = compileC expr env ++ compileLet' defs (argOffset 1 env)

@compileLet@ also uses @compileArgs@ to modify the offsets into the
stack for the compilation of the body, @e@.

> compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
> compileArgs defs env
>     = zip (map first defs) [n-1, n-2 .. 0] ++ argOffset n env
>             where n = length defs

> compileLetrec :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
> compileLetrec comp defs expr env
>   = [Alloc n] ++ concatMap c d ++ comp expr env' ++ [Slide n]
>     where env' = compileArgs defs env
>           n = length defs
>           c (i, e) = compileC e env' ++ [Update i]
>           d = zip [n-1, n-2 ..] $ snd $ unzip defs


\section{Mark 4: Adding primitives\index{G-machine!Mark 4}\index{arithmetic!in G-machine}}

\subsection{Data structures\index{data structures!G-machine Mark 4}}

We extend the G-machine state by adding a dump component. As previously
discussed, this is used to implement recursive calls to the evaluator.

The dump itself is a stack of @dumpItem@. Each of these is a pair
consisting of the instruction stream and stack to use when we resume
the original computation.

> type GmDump = [GmDumpItem]
> type GmDumpItem = (GmCode, GmStack)

> getDump :: GmState -> GmDump
> getDump (_, i, stack, dump, heap, globals, stats) = dump

> putDump :: GmDump -> GmState -> GmState
> putDump dump' (o, i, stack, dump, heap, globals, stats)
>    = (o, i, stack, dump', heap, globals, stats)

\subsection{Printing the state}

> showDump :: GmState -> Iseq
> showDump s
>     = iConcat       [iStr "  Dump:[",
>                      iIndent (iInterleave iNewline
>                             (map showDumpItem (reverse (getDump s)))),
>                      iStr "]"]

This in turn needs the function @showDumpItem@.

> showDumpItem :: GmDumpItem -> Iseq
> showDumpItem (code, stack)
>     = iConcat       [iStr "<",
>                      shortShowInstructions 3 code, iStr ", ",
>                      shortShowStack stack,         iStr ">"]

\par
We use the function @shortShowInstructions@ to print only the
first three instructions of the instruction stream in the dump items.
This is usually sufficient to indicate where the computation will
resume.

> shortShowInstructions :: Int -> GmCode -> Iseq
> shortShowInstructions number code
>     = iConcat [iStr "{", iInterleave (iStr "; ") dotcodes, iStr "}"]
>     where   codes   = map showInstruction (take number code)
>             dotcodes      | length code > number  = codes ++ [iStr "..."]
>                           | otherwise             = codes

Similarly, we do not need the full details of the stack component of
the dump item either, so we use @shortShowStack@.

> shortShowStack :: GmStack -> Iseq
> shortShowStack stack
>     = iConcat [iStr "[",
>            iInterleave (iStr ", ") (map (iStr . showaddr) stack),
>            iStr "]"]


We are now in a position to specify the rule for @Eval@. It saves the
remainder of the stack $s$ and the rest of the instructions $i$ as a
dump item on the dump. The new code sequence is just unwinding and the
new stack contains the singleton $a$.

> ev state = putCode [Unwind] $ putStack [a] $ putDump ((is,as):d) state
>     where is = getCode state
>           (a:as) = getStack state
>           d = getDump state

Let us write the boxing operations first. @boxInteger@ takes a number
and an initial state, and returns a new state in which the number has
been placed into the heap, and a pointer to this new node left on top
of the stack.

> boxInteger :: Int -> GmState -> GmState
> boxInteger n state
>       = putStack (a: getStack state) (putHeap h' state)
>       where (h', a) = hAlloc (getHeap state) (NNum n)

\par
Now to extract an integer at address @a@ from a state, we will use
@unboxInteger@.

> unboxInteger :: Addr -> GmState -> Int
> unboxInteger a state
>       = ub (hLookup (getHeap state) a)
>       where   ub (NNum i) = i
>               ub n        = error "Unboxing a non-integer"

\par
A generic {\em monadic operator\/}\index{monadic arithmetic operator} can now be
specified in terms of its boxing function, @box@, its unboxing
function @unbox@, and the operator @op@ on the unboxed values.

> primitive1 :: (b -> GmState -> GmState)  -- boxing function
>          -> (Addr -> GmState -> a)       -- unbixing function
>          -> (a -> b)                     -- operator
>          -> (GmState -> GmState)         -- state transition
> primitive1 box unbox op state
>    = box (op (unbox a state)) (putStack as state)
>    where (a:as) = getStack state

\par
The generic {\em dyadic operators\/}\index{dyadic arithmetic operator}
can now be implemented in a similar way using @primitive2@.

> primitive2 :: (b -> GmState -> GmState)  -- boxing function
>          -> (Addr -> GmState -> a)       -- unboxing function
>          -> (a -> a -> b)                -- operator
>          -> (GmState -> GmState)         -- state transition
> primitive2 box unbox op state
>    = box (op (unbox a0 state) (unbox a1 state)) (putStack as state)
>    where (a0:a1:as) = getStack state

\par
To be even more explicit, @arithmetic1@ implements all {\em monadic
arithmetic}\index{monadic arithmetic operator}, and @arithmetic2@
implements all {\em dyadic arithmetic}\index{dyadic arithmetic
operator}.

> arithmetic1 ::   (Int -> Int)            -- arithmetic operator
>                  -> (GmState -> GmState) -- state transition
> arithmetic1 = primitive1 boxInteger unboxInteger

> arithmetic2 ::   (Int -> Int -> Int)     -- arithmetic operation
>                  -> (GmState -> GmState) -- state transition
> arithmetic2 = primitive2 boxInteger unboxInteger

To make the use of @primitive2@ possible, we define @boxBoolean@

> boxBoolean :: Bool -> GmState -> GmState
> boxBoolean b state
>    = putStack (a: getStack state) (putHeap h' state)
>    where (h',a) = hAlloc (getHeap state) (NConstr b' [])
>          b' | b = 2              -- 2 is tag of True
>             | otherwise = 1      -- 1 is tag of False

Using this definition we can write a generic comparison function,
which we call @comparison@. This function takes a {\em
boxing}\index{boxing function} function for the booleans, the unboxing
function for integers (@unboxInteger@), and a comparison operator; it
returns a state transition.

> comparison :: (Int -> Int -> Bool) -> GmState -> GmState
> comparison = primitive2 boxBoolean unboxInteger

\subsection{The compiler\index{G-machine compiler!Mark 4}}

\begin{exercise}\label{gm:X:evalForUnwind}
Why has the initial instruction sequence been changed? What happens if
we retain the old one?
\end{exercise}

The simplest way to extend the compiler is simply to add G-machine
code for each of the new built-in functions to the
@compiledPrimitives@.  The initial four instructions of the sequence
ensure that the arguments have been evaluated to integers.

> compiledPrimitives :: [GmCompiledSC]
> compiledPrimitives -- The instruction sequences imply that we evaluate rhs before lhs
>    =       [("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind]),
>             ("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind]),
>             ("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind]),
>             ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind]),

We also need to add the negation function. As this only takes one
argument, we only evaluate one argument.

>             ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind]),

\par
The comparison operations are implemented as follows.

>             ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind]),
>             ("~=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind]),
>             ("<",  2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind]),
>             ("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind]),
>             (">",  2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind]),
>             (">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind])]

> compileE (ELet recursive defs e) args 
>          | recursive     = compileLetrec compileE defs e args
>          | otherwise     = compileLet    compileE defs e args
> compileE (EAp (EVar "negate") e) env = compileE e env ++ [Neg]
> compileE (EAp (EAp (EAp (EVar "if") cond) truebr) falsebr) env =
>     compileE cond env ++ [Casejump $ compileAlts compileE' alts env]
>              where alts = [(1, [], falsebr), (2, [], truebr)]
> compileE (EAp (EAp (EVar op) e1) e2) env
>     | op `elem` ops = compileE e2 env ++ compileE e1 (argOffset 1 env) ++ [i]
>     where ops = fst $ unzip builtInDyadic
>           i = aLookup builtInDyadic op (error "Can't happen")
> compileE (ECase e alts) env = compileE e env ++ [Casejump $ compileAlts compileE' alts env]
> compileE e env = let x = compileC e env
>                      whnf = case last x of
>                               Pushint _ -> True
>                               Pack _ _ -> True
>                               _ -> False
>                  in 
>                  if whnf then x else x ++ [Eval]

> builtInDyadic :: ASSOC Name Instruction
> builtInDyadic
>     =       [("+", Add), ("-", Sub), ("*", Mul), ("div", Div),
>              ("==", Eq), ("~=", Ne), (">=", Ge),
>              (">",  Gt), ("<=", Le), ("<",  Lt)]


\section{Mark 6: Adding data structures\index{data structures!in G-machine Mark 6}}

\subsection{Data structures\index{data structures!G-machine Mark 6}}

It would be nice to allow the machine to return values which are not
just numbers. We would like to be able to return values that consist
of constructors. This will require us to evaluate the components of
the structure recursively, and then return these values. To do this we
need to add yet another component to the state: @gmOutput@. This will
hold the result of the program.

> type GmState =
>    (GmOutput,              -- Current Output
>     GmCode,                -- Current Instruction Stream
>     GmStack,               -- Current Stack
>     GmDump,                -- The Dump
>     GmHeap,                -- Heap of Nodes
>     GmGlobals,             -- Global addresses in Heap
>     GmStats)               -- Statistics

This component is defined to be a character string.

> type GmOutput = [Char]

\par
We can write the access functions in the obvious way.

> getOutput :: GmState -> GmOutput
> getOutput (o, i, stack, dump, heap, globals, stats) = o

> putOutput :: GmOutput -> GmState -> GmState
> putOutput o' (o, i, stack, dump, heap, globals, stats)
>      = (o', i, stack, dump, heap, globals, stats)

To support constructor nodes in the heap, we augment the type @node@
with @NConstr@; this takes a positive number which will represent a
{\em tag}\index{tag!of constructor}, and a list of {\em
components}\index{components!of constructor} which we represent as the
list of the addresses of the nodes in heap.

> data Node
>           = NNum Int              -- Numbers
>           | NAp Addr Addr         -- Applications
>           | NGlobal Int GmCode    -- Globals
>           | NInd Addr
>           | NConstr Int [Addr]    -- Tag, list of components

\subsection{Printing the result}

Because we have a new state component which we wish to display, we
must redefine the function @showState@.

> showState :: GmState -> Iseq
> showState s
>      = iConcat [showOutput s,
>              showStack s,                  iNewline,
>              showDump s,                   iNewline,
>              showInstructions (getCode s), iNewline]

The @showOutput@ function is easy, because the output component is already a
string.

> showOutput :: GmState -> Iseq
> showOutput s = let x = getOutput s
>                in case x of
>                     [] -> INil
>                     _ -> iConcat [iStr "Output:", iStr x, iNewline]

\subsection{The instruction set}

> data Instruction 
>    = Slide Int
>    | Alloc Int
>    | Update Int
>    | Pop Int
>    | Unwind
>    | Pushglobal Name
>    | Pushint Int
>    | Push Int
>    | Mkap
>    | Eval
>    | Add | Sub | Mul | Div
>    | Neg
>    | Eq | Ne | Lt | Le | Gt | Ge

The four new instructions that are added to the machine are as follows:

>    | Pack Int Int
>    | Casejump [(Int, GmCode)]
>    | Split Int
>    | Print
>      deriving Show

> pack t n state = putHeap heap' (putStack (a:rest) state)
>       where (heap', a) = hAlloc (getHeap state) (NConstr t top)
>             s = getStack state
>             (top, rest) = if length s < n then error "Unsaturated constructor!"
>                           else splitAt n s

> cjmp brs state = putCode (i' ++ getCode state) state
>     where (a:_) = getStack state
>           (NConstr t _) = hLookup (getHeap state) a
>           i' = aLookup brs t $ error "Unexpected tag"

> split n state = putStack (as ++ s) state
>     where (a:s) = getStack state
>           (NConstr _ as) = hLookup (getHeap state) a

> prn state = go x
>     where (a:s) = getStack state
>           x = hLookup (getHeap state) a
>           go (NNum n) = putOutput (getOutput state ++ " " ++ show n) $ putStack s state
>           go (NConstr t as) = putOutput (getOutput state ++ " " ++ o) $ putCode (i' ++ getCode state) $ putStack (as ++ s) state
>               where i' = concat $ take n $ repeat [Eval, Print]
>                     n = length as
>                     o = "Pack{" ++ show t ++ "," ++ show n ++ "}"

\subsection{The compiler\index{G-machine compiler!Mark 6}}

> compileAlts ::    (Int -> GmCompiler)     -- compiler for alternative bodies
>                   -> [CoreAlt]            -- the list of alternatives
>                   -> GmEnvironment        -- the current environment
>                   -> [(Int, GmCode)]      -- list of alternative code sequences
> compileAlts comp alts env
>  = [(tag, comp (length names) body (zip names [0..] ++ argOffset (length names) env))
>          | (tag, names, body) <- alts]

\par
The @compileE'@ scheme is a small modification to the @compileE@
scheme. It simply places a @Split@ and @Slide@ around the code
generated by the ordinary @compileE@ scheme.

> compileE' :: Int -> GmCompiler
> compileE' offset expr env
>     | offset == 0 = Split 0 : x
>     | otherwise = [Split offset] ++ x ++ [Slide offset]
>     where x = compileE expr env
