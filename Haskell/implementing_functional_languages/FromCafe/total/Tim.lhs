> module Tim where
> import Utils
> import Language

\section{Mark 1: A minimal TIM}

> compile     :: CoreProgram -> TimState
> eval        :: TimState -> [TimState]
> showResults :: [TimState] -> [Char]

> shortRun :: FilePath -> IO String
> shortRun f = readFile f >>= (return . showResults . eval . compile . parseCore)

\par
It is often convenient to see all the intermediate states, so we
also provide @fullRun@, which uses @showFullResults@ to show each state:

> runProg :: FilePath -> IO String
> runProg f = readFile f >>= (return . showFullResults . eval . compile . parseCore)

\subsection{Data type definitions}

The type of addressing modes, @timAMode@,
is separated out as a distinct data type to stress the
relationship between @Push@ and @Enter@.

> data TimAMode = Arg Int
>               | Label String Int
>               | Code [Instruction]
>               | IntConst Int
>               | Data Int
>                 deriving Show

The state of the TIM machine is given by the following definition:

> type TimState = (String,               -- Output
>                  [Instruction],        -- The current instruction stream
>                  FramePtr,             -- Address of current frame
>                  FramePtr,             -- Data Frame
>                  TimStack,             -- Stack of arguments
>                  TimValueStack,        -- Value stack (not used yet)
>                  TimDump,              -- Dump (not used yet)
>                  TimHeap,              -- Heap of frames
>                  CodeStore,            -- Labelled blocks of code
>                  TimStats)             -- Statistics

We consider the representation for each of these components in turn.
\begin{itemize}
\item
The {\em current instruction stream\/} is represented by a list of instructions.
In a real machine this would be the program counter together with
the program memory.

\item
The {\em frame pointer\/}\index{frame pointer} is usually the
address of a frame in the heap, but there are two other possibilities:
it might be used to hold an integer value, or it might be
uninitialised.  The machine always `knows' which of these three
possibilities to expect, but it is
convenient in our implementation to distinguish them by using an
algebraic data type for @framePtr@:

> data FramePtr = FrameAddr Addr         -- The address of a frame
>               | FrameInt Int           -- An integer value
>               | FrameNull              -- Uninitialised
>                 deriving Show

If we do not do this, Miranda will (legitimately) complain
of a type error when we try to use an address as a number.
Furthermore, having a constructor for the uninitialised state @FrameNull@
means that our interpreter will discover if we ever mistakenly try to use
an uninitialised value as a valid address.

\item
The {\em stack\index{stack!in TIM\/}} contains {\em closures}, each of which is a
pair containing a code pointer and a frame pointer.
We represent the stack as a list.

> type TimStack = [Closure]
> type Closure = ([Instruction], FramePtr)

\item
The {\em heap\/}\index{heap} contains
{\em frames}, each of which is a tuple of closures.
The data type of frames is important enough to merit
an abstract data type of its own.

> type TimHeap = Heap Frame
>
> fAlloc   :: TimHeap -> [Closure] -> (TimHeap, FramePtr)
> fGet     :: TimHeap -> FramePtr -> Int -> Closure
> fUpdate  :: TimHeap -> FramePtr -> Int -> Closure -> TimHeap
> fList    :: Frame -> [Closure]           -- Used when printing

These operations allow frames to be built, and components to
be extracted and updated.
The first element of the list given to @fAlloc@ is numbered @1@ for
the purposes of @fGet@ and @fUpdate@.
Here is a simple implementation based on lists.

> type Frame = [Closure]
>
> fAlloc heap xs = (heap', FrameAddr addr)
>                  where
>                  (heap', addr) = hAlloc heap xs
>
> fGet heap (FrameAddr addr) n = f !! (n-1)
>                                where
>                                f = hLookup heap addr
>
> fUpdate heap (FrameAddr addr) n closure
>  = hUpdate heap addr new_frame
>    where
>    frame = hLookup heap addr
>    new_frame = take (n-1) frame ++ [closure] ++ drop n frame
>
> fList f = f

> type CodeStore = (FramePtr,
>                   [String]) -- The list of global names, for debugging only

\item
As usual, we make the {\em statistics\/} into an abstract data type which we
can add to easily:

> statInitial  :: TimStats
> statIncSteps :: TimStats -> TimStats
> statGetSteps :: TimStats -> Int

\end{itemize}
The first implementation, which counts only the number of steps,
is rather simple:

> type TimStats = Int           -- The number of steps
> statInitial = 0
> statIncSteps s = s+1
> statGetSteps s = s


\subsection{Compiling a program}
\label{sect:tim:compiler}

@compile@ works very much like the template instantiation compiler,
creating an initial machine state from the program it is given.
The main difference lies in the compilation function @compileSC@ which
is applied to each supercombinator.

> compile program
>     = ([],
>        [Enter $ compileV "main" initial_env],     -- Initial instructions
>        FrameNull,                  -- Null frame pointer
>        FrameNull,
>        initialArgStack,            -- Argument stack
>        initialValueStack,          -- Value stack
>        initialDump,                -- Dump
>        initialHeap,
>        cstore,              
>        statInitial)                -- Initial statistics
>        where
>        sc_defs          = program ++ preludeDefs ++ parseCore extraDefs
>        compiled_sc_defs = map (compileSC initial_env) sc_defs
>        compiled_code    = compiled_sc_defs ++ compiledPrimitives
>        initial_env = buildInitialEnv indexed_code
>        indexed_code = zip [1..] compiled_code
>        extraDefs = "if c t f = case c of <1> -> f; <2> -> t;\
>                    \nil = Pack{1,0};\
>                    \cons = Pack{2,2};\
>                    \MkPair = Pack{1,2};\
>                    \fst x = case x of <1> a b -> a;\
>                    \snd x = case x of <1> a b -> b"
>        (initialHeap, cstore) = allocateInitialHeap indexed_code

\par
\sloppy
The compiled supercombinators, @compiled_sc_defs@, is obtained by compiling
each of the supercombinators in the program, using @compileSC@.
The initial environment passed to @compileSC@ gives a suitable addressing
mode for each supercombinator.
The code store, @compiled_code@, is obtained by combining @compiled_sc_defs@
with @compiledPrimitives@.

It's not necessary to hand-craft code for built-in operators, as they
are always saturated (i.e. unlike Haskell, our grammar doesn't allow
them to be partially applied), and will be inlined.  However, negate
is a named function so partially applying negate is perfectly legal in
our grammar.  We must define it here.

> compiledPrimitives = [("negate",[UpdateMarkers 1,
>                                  Take 1 1,
>                                  Push $ Code [Op Neg, Return],
>                                  Enter $ Arg 1])
>                      ]

                      ("+",[Take 2 2,
                            Push $ Code [Push $ Code [Op Add, Return],
                                         Enter $ Arg 1],
                            Enter $ Arg 2]),
                      ("-",[Take 2 2,
                            Push $ Code [Push $ Code [Op Sub, Return],
                                         Enter $ Arg 1],
                            Enter $ Arg 2]),
                      ("*",[Take 2 2,
                            Push $ Code [Push $ Code [Op Mul, Return],
                                         Enter $ Arg 1],
                            Enter $ Arg 2]),
                      ("/",[Take 2 2,
                            Push $ Code [Push $ Code [Op Div, Return],
                                         Enter $ Arg 1],
                            Enter $ Arg 2])
--                      ("==",[Op Eq]),("~=",[Op Ne]),(">=",[Op Ge]),(">",[Op Gt]),("<=",[Op Le]),("<",[Op Lt])
                     ]

Unlike the template machine and the G-machine, the initial heap is empty.
The reason for a non-empty initial heap in those cases was to retain sharing
for CAFs\index{CAF} (that is, supercombinators with no arguments
-- Section~\ref{sect:caf}).
In this initial version of the TIM machine, the compiled TIM code for a CAF
will be executed each time it is called, so the work
of evaluating the CAF is not shared.  We will address this problem much later,
in Section~\ref{sect:tim:caf}.

The heart of the compiler is a direct translation of the compilation
schemes \tSC{}, \tR{} and \tA{} into the functions
@compileSC@, @compileR@ and @compileA@ respectively.
The environment, $\rho$, is represented by an association list binding
names to addressing modes.  The G-machine compiler used a mapping from
names to stack offsets, but the extra flexibility of using
addressing modes turns out to be rather useful.

> type TimCompilerEnv = [(Name, TimAMode)]

\par
Now we are ready to define @compileSC@:

> compileSC :: TimCompilerEnv -> CoreScDefn -> (Name, [Instruction])
> compileSC env (name, args, body)
>  = (name, x) 
>     where
>     (d, instructions) = compileR body new_env n
>     new_env = (zip args (map Arg [1..])) ++ env
>     n = length args
>     x | d == 0 = instructions
>       | n == 0 = Take d 0 : instructions
>       | otherwise = UpdateMarkers n : Take d n : instructions

@compileR@ takes an expression and an environment, and delivers a list
of instructions:

> compileR :: CoreExpr -> TimCompilerEnv -> Int -> (Int, [Instruction])

> compileR x env d | canInline x = compileB x env d [Return]
>                      -- Although awkwardly, we must ensure they are fully applied
>                where canInline (ENum _) = True
>                      canInline (EAp (EVar "negate") _) = True
>                      canInline (EAp (EAp (EVar op) _) _) | elem op builtInBinOpName = True
>                      canInline (EAp (EAp (EAp (EVar "if") _) _) _) = True
>                      canInline _ = False

> compileR (EVar v) env d = (d, mkEnter $ compileV v env)
>           -- Optimization: Enter (Code i) is equivalent to i
>     where mkEnter (Code i) = i
>           mkEnter other_am = [Enter other_am]

> compileR (EAp e (EVar a)) env d = (d1, (Push $ compileV a env) : is)
>     where (d1, is) = compileR e env d
> compileR (EAp e (ENum n)) env d = (d1, (Push $ IntConst n) : is)
>     where (d1, is) = compileR e env d

> compileR (EAp fun arg) env d = (d2, Move (d+1) am : (Push $ Code [Enter $ Arg $ d+1]) : is)
>     where (d1, am) = compileU arg (d+1) env (d+1)
>           (d2, is) = compileR fun env d1

> compileR (ELet recursive defs e) env d =
>     (d', ii ++ is)
>     where (dn, ii) = mapAccuml f (d + length defs) $ zip [d+1..] exps
>           f :: Int -> (Int, CoreExpr) -> (Int, Instruction)
>           f dd (j,ee)
>               | recursive = let (x,y) = compileU ee j env' dd in
>                             (x, Move j y)
>               | otherwise = let (x,y) = compileU ee j env dd in
>                             (x, Move j y)
>           env' = (zip names (map mkIndMode [d+1 ..])) ++ env
>           (names, exps) = unzip defs
>           (d', is) = compileR e env' dn
>           mkIndMode n = Code [Enter $ Arg n]

> compileR (ECase e alts) env d = (d', (Push $ Code [Switch bb]) : is)
>     where (dd, bb) = unzip $ map compileE alts
>           (d', is) = compileR e env $ maximum dd
>           -- We optimize bound variable accessing according to Sec 4.6.5
>           compileE (t, xs, body) = (d'', (t, (ism ++ isb)))
>               where (d'', ism) = case body of
>                                            ENum _ -> (d, [])
>                                            EVar _ -> (d, [])
>                                            _ -> (d', take n $ zipWith Move [d+1 ..] $ map Data [1..])
>                     n = length xs
>                     (d', isb) = compileR body env' (d+n)
>                     env' = case body of
>                              EVar _ -> (zip xs $ map Data [1 ..]) ++ env
>                              _ -> (zip xs $ map Arg [d+1 ..]) ++ env

> -- We emit "Take 0 0" so that Pack{1,0} will be recognized correctly when printed.
> compileR (EConstr t 0) _ d = (d, [Take 0 0, ReturnConstr t])
> compileR (EConstr t n) _ d = (d, [UpdateMarkers n, Take n n, ReturnConstr t])

> compileV v env = aLookup env v $ error ("Unknown variable " ++ v)

\subsection{The evaluator}

Next we need to define how the evaluator actually works.
The definition of
@eval@ is exactly as for the template instantiation machine:

> eval state
>  = state : rest_states  where
>                         rest_states | timFinal state = []
>                                     | otherwise      = eval next_state
>                         next_state  = doAdmin (step state)
>
> doAdmin state = applyToStats statIncSteps state

The @timFinal@ function says when a state is a final state.
We could invent a @Stop@ instruction, but it
is just as easy
to say that we have finished when the code sequence is empty:

> timFinal (_, [], frame, _, stack, vstack, dump, heap, cstore, stats) = True
> timFinal _                                                 = False

The @applyToStats@ function just applies a function to the
statistics component of the state:

> applyToStats stats_fun (o, instr, frame, fd, stack, vstack,
>                         dump, heap, cstore, stats)
>  = (o, instr, frame, fd, stack, vstack, dump, heap, cstore, stats_fun stats)


\subsubsection{Taking a step}

@step@ does the case analysis which takes a single instruction and
executes it. The @Take@ equation is a
straightforward transliteration of the corresponding state transition rule
(\ref{rule:take}):

> step :: TimState -> TimState

> step (o, Take t n:instr, fptr, fd, stack, vstack, dump, heap, cstore,stats)
>  | length stack >= n = (o, instr, fptr', fd, bottom, vstack, dump, heap', cstore, stats)
>  | otherwise         = error "Impossible"
>    where (heap', fptr') = fAlloc heap (top ++ dummy)
>          (top, bottom) = splitAt n stack
>          dummy = replicate (t - n) ([], FrameNull)

The equations for @Enter@ and @Push@ take advantage of the @Push@/@Enter@
relationship\index{Push/Enter relationship@@@Push@/@Enter@ relationship}
by using a common function @amToClosure@ which converts
a @timAMode@ to a closure:

> step (o, [Enter am], fptr, fd, stack, vstack, dump, heap, cstore, stats)
>  = (o, instr', fptr', fd, stack, vstack, dump, heap, cstore, stats)
>    where (instr',fptr') = amToClosure am f heap cstore
>          f = case am of
>                Data _ -> fd
>                _ -> fptr

> step (o, Push am:instr, fptr, fd, stack, vstack, dump, heap, cstore, stats)
>  = (o, instr, fptr, fd, amToClosure am fptr heap cstore : stack,
>     vstack, dump, heap, cstore, stats)

> step (o, Op Neg:instr, fptr, fd, stack, (n:vstack), dump, heap, cstore, stats)
>  = (o, instr, fptr, fd, stack, (-n):vstack, dump, heap, cstore, stats)
> step (o, Op op:instr, fptr, fd, stack, (x:y:vstack), dump, heap, cstore, stats)
>  = (o, instr, fptr, fd, stack, (i x y):vstack, dump, heap, cstore, stats)
>    where i = aLookup builtInBinOpR op (error "Unexpected operator")

> step (o, [Return], fptr, fd, [], vstack@(n:_), (f',x,s):dump, heap, cstore, stats)
>  = (o, [Return], fptr, fd, s, vstack, dump, heap', cstore, stats)
>    where heap' = fUpdate heap f' x (intCode, FrameInt n)
> step (o, [Return], fptr, _, (instr', fptr'):stack, vstack, dump, heap, cstore, stats)
>  = (o, instr', fptr', FrameNull, stack, vstack, dump, heap, cstore, stats)

> step (o, PushV FramePtr:instr, fptr@(FrameInt n), fd, stack, vstack, dump, heap, cstore, stats)
>  = (o, instr, fptr, fd, stack, n:vstack, dump, heap, cstore, stats)
> step (o, PushV (IntVConst n):instr, fptr, fd, stack, vstack, dump, heap, cstore, stats)
>  = (o, instr, fptr, fd, stack, n:vstack, dump, heap, cstore, stats)

> step (o, Move i am:instr, fptr, fd, stack, vstack, dump, heap, cstore, stats)
>  = (o, instr, fptr, fd, stack, vstack, dump, heap', cstore, stats)
>    where c = amToClosure am f heap cstore
>          heap' = fUpdate heap fptr i c
>          f = case am of
>                Data _ -> fd
>                _ -> fptr

> step (o, PushMarker x:instr, fptr, fd, stack, vstack, dump, heap, cstore, stats)
>  = (o, instr, fptr, fd, [], vstack, (fptr,x,stack):dump, heap, cstore, stats)

> step (o, i@(UpdateMarkers n:instr), fptr, fd, stack, vstack, [], heap, cstore, stats)
>  | m > n = (o, instr, fptr, fd, stack, vstack, [], heap, cstore, stats)
>  | otherwise = error "Too few args for a supercombinator"
>     where m = length stack
> step (o, i@(UpdateMarkers n:instr), fptr, fd, stack, vstack, dump@((fu,x,s):d), heap, cstore, stats)
>  | m >= n = (o, instr, fptr, fd, stack, vstack, dump, heap, cstore, stats)
>  | otherwise = (o, i, fptr, fd, stack ++ s, vstack, d, heap', cstore, stats)
>     where (h', f') = fAlloc heap stack
>           heap' = fUpdate h' fu x (i', f')
>           i' = map (Push . Arg) [m, m-1 .. 1] ++ i
>           m = length stack

> step (o, [Switch brs], fptr, fd, stack, (t:vstack), dump, heap, cstore, stats)
>  = (o, i, fptr, fd, stack, vstack, dump, heap, cstore, stats)
>    where i = aLookup brs t $ error "Unexpected tag"

> step (o, i@[ReturnConstr t], fptr, fd, [], vstack, (f',x,s):dump, heap, cstore, stats)
>  = (o, i, fptr, fd, s, vstack, dump, heap', cstore, stats)
>    where heap' = fUpdate heap f' x (i, fptr)
> step (o, [ReturnConstr t], fptr, fd, (instr', fptr'):stack, vstack, dump, heap, cstore, stats)
>  = (o, instr', fptr', fptr, stack, t:vstack, dump, heap, cstore, stats)

This is my own Print instruction, because Section 4.6.4 only supports lists.
Print always immediately follow Return/ReturnConstr.
If the data frame is null, the previous instruction was Return and the top of the value stack is a number.
Otherwise, we just evaluated the WHNF of a node Pack{tag,arity}.
In this case, we should start evaluating its components, by pushing a print continuation and enter the component,
which is packed in a frame pointed to by the data frame pointer.

However, there is a flaw!
When we evaluate Pack{tag,n} Arg_1 ... Arg_n, Arg_i itself may be a data structure, which would change the data frame pointer,
making evaluate Arg_(i+1) impossible.
We restore the data frame pointer after evaluating each component, using a new instruction.

> step (o, Print:i, fptr, FrameNull, stack, (t:vstack), dump, heap, cstore, stats)
>  = (o ++ show t ++ " ", i, fptr, FrameNull, stack, vstack, dump, heap, cstore, stats)
> step (o, Print:i, fptr, fd@(FrameAddr a), stack, (t:vstack), dump, heap, cstore, stats)
>  = (o ++ o', expandI 1, fptr, fd, stack, vstack, dump, heap, cstore, stats)
>    where o' = "Pack{" ++ show t ++ "," ++ show n ++"} "
>          n = length $ hLookup heap a
>          expandI x | x > n = i
>                    | otherwise = [Push $ Code $ Print : OverrideDataFrame fd: expandI (x+1), Enter $ Data x]

> step (o, OverrideDataFrame f:i, fptr, _, stack, vstack, dump, heap, cstore, stats)
>  = (o, i, fptr, f, stack, vstack, dump, heap, cstore, stats)

@amToClosure@ delivers the closure addressed by the addressing mode
which is its first argument:

> amToClosure :: TimAMode -> FramePtr -> TimHeap -> CodeStore -> Closure
> amToClosure (Arg n)      fptr heap _ = fGet heap fptr n
> amToClosure (Data n)      fptr heap _ = fGet heap fptr n
> amToClosure (Code il)    fptr _ _ = (il, fptr)
> amToClosure (Label l k) _ heap (cstore,_) = fGet heap cstore k
> amToClosure (IntConst n) _ _ _ = (intCode, FrameInt n)

\subsection{Printing the results}

As with the template instantiation version we need a rather boring
collection of functions to print the results in a sensible way.
It is often useful to print out the supercombinator definitions, so
@showResults@ begins by doing so, using the definitions in the first
state:

> showFullResults states
>  = iDisplay (iConcat [
>        iStr "Supercombinator definitions", iNewline, iNewline,
>        showSCDefns first_state, iNewline, iNewline,
>        iStr "State transitions", iNewline,
>        iLayn (map showState states), iNewline, iNewline,
>        showStats (last states)
>    ])
>    where
>    (first_state:rest_states) = states

@showResults@ just shows the last state and some statistics:

> showResults states
>  = iDisplay (iConcat [
>     showState last_state, iNewline, iNewline, showStats last_state
>    ])
>    where last_state = last states

\par
The rest of the functions are straightforward.  @showSCDefns@ displays
the code for each supercombinator.

> showSCDefns :: TimState -> Iseq
> showSCDefns (_, instr, fptr, _, stack, vstack, dump, heap, (cstore,names), stats)
>  = iInterleave iNewline $ map showSC $ zip names ils
>     where showSC (name, il)
>               = iConcat [
>                  iStr "Code for ", iStr name, iStr ":", iNewline,
>                  iStr "   ", showInstructions Full il, iNewline, iNewline
>                 ]
>           ils = map (fst . (fGet heap cstore)) [1..]


@showState@ displays a TIM machine state.

> showState :: TimState -> Iseq
> showState (out, instr, fptr, fd, stack, vstack, dump, heap, cstore, stats)
>  = iConcat [
>     iStr "Code:  ", showInstructions Terse instr, iNewline,
>     showFrame heap fptr,
>     showFrame heap fd,
>     showStack stack,
>     showValueStack vstack,
>     showDump dump,
>     showOut out,
>     iNewline
>    ]

> showOut [] = INil
> showOut x = iConcat [iStr "Output: ", iStr x, iNewline]

@showFrame@ shows the frame component of a state, using @showClosure@
to display each of the closures inside it.

> showFrame :: TimHeap -> FramePtr -> Iseq
> showFrame heap FrameNull = iStr "Null frame ptr" `iAppend` iNewline
> showFrame heap (FrameAddr addr)
>  = iConcat [
>        iStr "Frame @",
>        iNum addr,
>        iStr " : <",
>        iIndent (iInterleave iNewline
>                             (map showClosure (fList (hLookup heap addr)))),
>        iStr ">", iNewline
>    ]
> showFrame heap (FrameInt n)
>  = iConcat [ iStr "Frame ptr (int): ", iNum n, iNewline ]

@showStack@ displays the argument stack, using @showClosure@ to display each
closure.

> showStack :: TimStack -> Iseq
> showStack stack
>  = iConcat [   iStr "Arg stack: [",
>                iIndent (iInterleave iNewline (map showClosure stack)),
>                iStr "]", iNewline
>    ]

> showValueStack :: TimValueStack -> Iseq
> showValueStack stack
>  = iConcat [   iStr "Value stack: [",
>                iIndent (iInterleave iNewline (map iNum stack)),
>                iStr "]", iNewline
>    ]

> showDump :: TimDump -> Iseq
> showDump dump
>  = iConcat [   iStr "Dump: [",
>                iIndent (iInterleave iNewline (map showDumpItem dump)),
>                iStr "]", iNewline
>    ]
>     where showDumpItem (f, i, s) = iConcat $ map iStr $ [ "(", show f, ", ",
>                                                           show i, ", <stack>" ]

@showClosure@ displays a closure, using @showFramePtr@ to display
the frame pointer.

> showClosure :: Closure -> Iseq
> showClosure (i,f)
>  = iConcat [   iStr "(",  showInstructions Terse i,  iStr ", ",
>                showFramePtr f,  iStr ")"
>    ]

> showFramePtr :: FramePtr -> Iseq
> showFramePtr FrameNull = iStr "null"
> showFramePtr (FrameAddr a) = iStr (show a)
> showFramePtr (FrameInt n) = iStr "int " `iAppend` iNum n

@showStats@ is responsible for printing out accumulated statistics:

> showStats :: TimState -> Iseq
> showStats (_, instr, fptr, _, stack, vstack, dump, heap, code, stats)
>  = iConcat [ iStr "Steps taken = ", iNum (statGetSteps stats), iNewline,
>              iStr "No of frames allocated = ", iNum (hSize heap),
>              iNewline
>    ]

\subsubsection{Printing instructions}

We are going to need to print instructions and instruction sequences.
If a sequence of instructions is printed as one long line, it is
rather hard to read, so it is worth writing some code to pretty-print
them.

In fact we want to be able to print either the entire
code for an instruction sequence (for example when printing a supercombinator
definition), or just some abbreviated form of it.
An example of the latter occurs when printing the contents of the stack;
it can be helpful to see some part of the code in each closure, but we do not
want to see it all!  Accordingly, we give an extra argument, @d@, to each
function to tell it how fully to print.
The value of this argument is either
@Full@, @Terse@ or @None@.

> data HowMuchToPrint = Full | Terse | None

\par
@showInstructions@ turns a list of instructions into an @iseq@.
When @d@ is @None@, only an ellipsis is printed.
If @d@ is @Terse@, the instructions are printed all on one line, and
nested instructions are printed with @d@ as @None@.
If @d@ is @Full@, the instructions are laid out one per line, and printed
in full.

> showInstructions :: HowMuchToPrint -> [Instruction] -> Iseq
> showInstructions None il = iStr "{..}"
> showInstructions Terse il
>  = iConcat [iStr "{", iIndent (iInterleave (iStr ", ") body), iStr "}"]
>    where
>       instrs = map (showInstruction None) il
>       body | length il <= nTerse = instrs
>            | otherwise           = (take nTerse instrs) ++ [iStr ".."]
> showInstructions Full il
>  = iConcat [iStr "{ ", iIndent (iInterleave sep instrs), iStr " }"]
>    where
>    sep = iStr "," `iAppend` iNewline
>    instrs = map (showInstruction Full) il

@showInstruction@ turns a single instruction into an @iseq@.

> showInstruction d (Enter x) = (iStr "Enter ") `iAppend` (showArg d x)
> showInstruction d (Push x)  = (iStr "Push ")  `iAppend` (showArg d x)
> showInstruction d (Move x y) = iConcat [iStr "Move ", iNum x, iStr " ", showArg d y]
> showInstruction d (Switch brs) = iConcat [iStr "Switch [",
>                                           iIndent $ iInterleave iNewline (map f brs),
>                                           iStr "]"]
>     where f (l, is) = iConcat [iNum l, iStr " -> ", showInstructions d is]
> showInstruction _ x = iStr $ show x

> showArg d (Code il)    = (iStr "Code ")  `iAppend` (showInstructions d il)
> showArg _ x = iStr $ show x

@nTerse@ says how many instructions of a sequence should be
printed in terse form.

> nTerse = 3

\section{Mark 2: Adding arithmetic}

> intCode = [PushV FramePtr, Return]

\begin{itemize}
\item
Add the following type definition and initialisation for the value stack:

> type TimValueStack = [Int]
> initialValueStack = []

\item
Add the new instructions @PushV@, @Return@ and @Op@
to the @instruction@ data type.
TIM is no longer a three instruction machine!

> data Instruction = Take Int Int
>                  | Push TimAMode
>                  | PushV ValueAMode
>                  | Enter TimAMode
>                  | Return
>                  | Op Op
>                  | Move Int TimAMode
>                  | PushMarker Int
>                  | UpdateMarkers Int
>                  | Switch [(Int, [Instruction])]
>                  | ReturnConstr Int
>                  | Print
>                  | OverrideDataFrame FramePtr
>                    deriving Show
> data Op = Add  | Sub | Mul | Div | Neg
>         | Gt | Ge | Lt | Le | Eq | Ne
>           deriving (Show, Eq)

> builtInBinOp = [("+", Add), ("-", Sub), ("*", Mul), ("/", Div)
>                ,("==", Eq), ("~=", Ne), (">=", Ge)
>                ,(">",  Gt), ("<=", Le), ("<",  Lt)
>                ]
> builtInBinOpName = fst $ unzip builtInBinOp
> builtInBinOpR = zip (snd $ unzip builtInBinOp) x
>     where x = [(+), (-), (*), div] ++ map convert [(==), (/=), (>=), (>), (<=), (<)]
>           convert :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
>           convert f = \ a b -> if f a b then 2 else 1

So far the argument of a @PushV@ instruction can only be @FramePtr@,
but we will shortly add a second form which allows us to push literal
constants onto the value stack.  So it is worth declaring an algebraic
data type for @valueAMode@:

> data ValueAMode = FramePtr
>                 | IntVConst Int
>                   deriving Show

\item
Now that @intCode@ is no longer empty, we must initialise
the stack to have
a suitable continuation (return address) for @main@ to return to.
The way to do this is to make @compile@ initialise the stack with the
closure @([],FrameNull)@, by redefining @initialArgStack@:

> initialArgStack = [([Print], FrameNull)]

This continuation has an empty code sequence, so
the machine will now halt with the result on top of the value stack.
\end{itemize}

> compileB (EAp (EVar "negate") e) env d cont = compileB e env d $ Op Neg : cont
> compileB (EAp (EAp (EVar op) e1) e2) env d cont
>     | op `elem` ops = (max d1 d2, is2)
>     where ops = builtInBinOpName
>           i = aLookup builtInBinOp op (error "Can't happen")
>           (d1, is1) = compileB e1 env d $ Op i : cont
>           (d2, is2) = compileB e2 env d is1
> compileB (ENum n) env d cont = (d, PushV (IntVConst n) : cont)
> compileB (EAp (EAp (EAp (EVar "if") cond) truebr) falsebr) env d _ =
>     compileB cond env (max d1 d2) [x]
>         where (d1, t) = compileR truebr env d
>               (d2, f) = compileR falsebr env d
>               x = Switch [(1, f), (2, t)]
> compileB x env d cont = (d', Push (Code cont) : is)
>     where (d', is) = compileR x env d


\section{Mark 4: Updating}\index{updates!in TIM}

\begin{itemize}
\item
Give a type definition for the dump\index{dump!in TIM}.  It is just a stack of
triples, represented as a list, and initialised to be empty:

> type TimDump = [(FramePtr,  -- The frame to be updated
>                  Int,       -- Index of slot to be updated
>                  TimStack)  -- Old stack
>                ]
> initialDump = []

> mkUpdIndMode :: Int -> TimAMode
> mkUpdIndMode n = Code [PushMarker n, Enter (Arg n)]

\end{itemize}

> compileU (ENum n) _ _ d = (d, IntConst n)
> compileU e u env d = (d', Code (PushMarker u:is))
>     where (d', is) = compileR e env d


\section{Mark 6: Constant applicative forms and the code store\advanced}

> allocateInitialHeap :: [(Int, (Name, [Instruction]))] -> (TimHeap, CodeStore)
> allocateInitialHeap indexed_code
>  = (heap, (global_frame_addr, names))
>    where
>    closures = map f indexed_code
>    f (offset, (_, code)) = case code of
>                                      UpdateMarkers _ : _ -> (code, global_frame_addr)
>                                      _ -> (PushMarker offset : code, global_frame_addr) -- CAF
>    (heap, global_frame_addr) = fAlloc hInitial closures
>    names = [name | (_, (name, _)) <- indexed_code]


> buildInitialEnv :: [(Int, (Name, [Instruction]))] -> TimCompilerEnv
> buildInitialEnv = map f
>     where f (offset, (name, UpdateMarkers _ : _)) = (name, Label name offset)
>           f (offset, (name, _)) = (name, mkIndMode name offset)                      -- CAF
>           mkIndMode name n = Code [Enter $ Label name n]
