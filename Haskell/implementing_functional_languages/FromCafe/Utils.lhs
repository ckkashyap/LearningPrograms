>

% $Date: 91/09/10 14:49:21 $
% $Revision: 1.14 $
% (c) 1991 Simon Peyton Jones & David Lester.
\chapter{Utilities module}
\label{sect:utils}

This appendix gives definitions for various useful types and functions
used throughout the book.

> module Utils where

> -- The following definitions are used to make some synonyms for routines
> -- in the Gofer prelude to be more Miranda compatible
> shownum n = show n
> hd :: [a] -> a
> hd  = head                        -- in Gofer standard prelude
> tl :: [a] -> [a]
> tl  = tail                        -- in Gofer standard prelude
> zip2 :: [a] -> [b] -> [(a,b)]
> zip2  = zip                       -- in Gofer standard prelude
> -- can't do anything about # = length, since # not binary.

\section{The heap type}
\label{sect:heap}

The abstract data types @heap@ and @addr@ are used to represent the
GHarbage-collected heap\index{heap} of nodes\index{node}
for each of our implementations.

\subsection{Specification}

A @heap@ of @*@ is a collection of \stressD{objects} of type @*@, each
identified by a
unique {\em address\/} of type @addr@.
The following operations are provided:

> hInitial :: Heap a
> hAlloc   :: Heap a -> a -> (Heap a, Addr)
> hUpdate  :: Heap a -> Addr -> a -> Heap a
> hFree    :: Heap a -> Addr -> Heap a

@hInitial@ returns an initialised empty heap.
@hAlloc@ takes a heap and an object, and returns a new heap and an address;
the new heap is exactly the same as the old one, except that
the specified object is found at the address returned.
@hUpdate@ takes a heap, an address and an object;
it returns a new heap in which the address is now
associated with the object.
@hFree@ takes a heap and an address and returns a new heap with
the specified object removed.

> hLookup  :: Heap a -> Addr -> a
> hAddresses :: Heap a -> [Addr]
> hSize    :: Heap a -> Int

@hLookup@ takes a heap and an address and returns the
object associated with that address.
@hAddresses@ returns the addresses of all the objects in the heap.
@hSize@ returns the number of objects in the heap.

> hNull      :: Addr
> hIsnull    :: Addr -> Bool

@hNull@ is an address guaranteed to differ from every address returned
by @hAlloc@; @hIsnull@ tells whether an address is this distinguished
value.

Finally, we add a show function so that addresses can be printed easily.

> showaddr :: Addr -> [Char]

By giving it the name @show@ followed by the name of the type (@addr@),
we inform Miranda that when Miranda's built-in @show@ function encounters
an object of type @addr@, it should use @showaddr@ to convert it to a list
of characters.

\subsection{Representation}

The heap is represented as a triple, containing:
\begin{itemize}
\item the number of objects in the heap;
\item a list of unused addresses;
\item an association list mapping addresses to objects.
\end{itemize}
Addresses are represented as numbers.

> type Heap a = (Int, [Int], [(Int, a)])
> type Addr   = Int

\par
We implement the operations in a (fairly) obvious manner.

> hInitial                             = (0,      [1..],  [])
> hAlloc  (size, (next:free), cts) n   = ((size+1, free,   (next,n) : cts),next)
> hUpdate (size, free,        cts) a n = (size,   free,   (a,n) : remove cts a)
> hFree   (size, free,        cts) a   = (size-1, a:free, remove cts a)

> hLookup (size,free,cts) a
>  = aLookup cts a (error ("can't find node " ++ showaddr a ++ " in heap"))
>
> hAddresses (size, free, cts) = [addr | (addr, node) <- cts]
>
> hSize (size, free, cts) = size

> hNull    = 0
> hIsnull a = a == 0
> showaddr a = "#" ++ shownum a         -- Print # to identify addresses

\par
The auxiliary function @remove@ removes an item from a heap contents:

> remove :: [(Int,a)] -> Int -> [(Int,a)]
> remove [] a = error ("Attempt to update or free nonexistent address #" ++
>                      shownum a)
> remove ((a',n):cts) a | a == a' = cts
>                       | a /= a' = (a',n) : remove cts a

\section{The association list type}
\label{sect:assoc}

An \stressD{association list} associates {\em keys\/} to {\em values}.
It is represented by a list of (key,value) pairs, using a type synonym.
It is not an abstract type
because it turns out to be so convenient to use list-manipulation
operations on it.

> type ASSOC a b = [(a,b)]

You can use one association list, $e_1$, to extend another, $e_2$,
using ordinary list append, thus $e_1\ @++@\ e_2$.
A lookup in this extended environment
will search $e_1$ first and then $e_2$.

GHiven a key, $k$, you can find the associated value using @aLookup@.


The call $@aLookup@~alist~key~default$ searches the association list
$alist$ starting from the head of the list;
if it finds a $(key,val)$ pair it returns $val$,
otherwise it returns $default$.

> aLookup []         k' def           = def
> aLookup ((k,v):bs) k' def | k == k' = v
>                           | k /= k' = aLookup bs k' def

\par
The functions @aDomain@ and @aRange@ find the range and domain of the
association list, respectively:

> aDomain :: ASSOC a b -> [a]
> aDomain alist = [key | (key,val) <- alist]
>
> aRange :: ASSOC a b -> [b]
> aRange  alist = [val | (key,val) <- alist]

@aEmpty@ is the empty association list:

> aEmpty = []


\section{Generating unique names}

In Chapter~\ref{sect:lambda-lift} we need to generate unique
names\index{unique names}
for newly generated supercombinators.  The abstract data type @nameSupply@
acts as a supply of unique names.

> getName  :: NameSupply -> [Char]   -> (NameSupply, [Char])
> getNames :: NameSupply -> [[Char]] -> (NameSupply, [[Char]])
> initialNameSupply :: NameSupply

There are three operations.  @getName@ takes a name supply and a prefix
string, and returns a depleted name supply together with a string which is
a new unique name; this string has the specified prefix.  @getNames@ does the
same thing for a list of prefixes.  Finally, @initialNameSupply@ is the
initial, undepleted name supply.

\subsection{Representation}

A name supply is represented by a single integer.

> type NameSupply = Int
> initialNameSupply = 0
> getName name_supply prefix = (name_supply+1, makeName prefix name_supply)
> getNames name_supply prefixes
>  = (name_supply + length prefixes, zipWith makeName prefixes [name_supply..])

> makeName prefix ns = prefix ++ "_" ++ shownum ns

\section{Sets}

\label{sect:set}

The abstract data type of sets\index{sets} has the following signature.

> setFromList     :: (Ord a) => [a]     -> Set a
> setToList       :: (Ord a) => Set a   -> [a]
> setUnion        :: (Ord a) => Set a   -> Set a -> Set a
> setIntersection :: (Ord a) => Set a   -> Set a -> Set a
> setSubtraction  :: (Ord a) => Set a   -> Set a -> Set a
> setElementOf    :: (Ord a) => a       -> Set a -> Bool
> setEmpty        :: (Ord a) => Set a
> setIsEmpty      :: (Ord a) => Set a   -> Bool
> setSingleton    :: (Ord a) => a       -> Set a
> setUnionList    :: (Ord a) => [Set a] -> Set a

\subsection{Representation}

In this implementation, sets are represented by {\em ordered\/} lists.

> type Set a = [a]           -- Ordered by the sort function

The implementation of the operations is straightforward.

> setEmpty = []
> setIsEmpty s = null s
> setSingleton x = [x]

> setFromList = rmdup . sort
>                   where rmdup []       = []
>                         rmdup [x]      = [x]
>                         rmdup (x:y:xs) | x == y = rmdup (y:xs)
>                                        | x /= y = x: rmdup (y:xs)

> setToList xs = xs

> setUnion []     []            = []
> setUnion []     (b:bs)        = (b:bs)
> setUnion (a:as) []            = (a:as)
> setUnion (a:as) (b:bs) | a <  b  = a: setUnion as (b:bs)
>                        | a == b  = a: setUnion as bs
>                        | a >  b  = b: setUnion (a:as) bs

> setIntersection []     []     = []
> setIntersection []     (b:bs) = []
> setIntersection (a:as) []     = []
> setIntersection (a:as) (b:bs) | a <  b = setIntersection as (b:bs)
>                               | a == b = a: setIntersection as bs
>                               | a >  b = setIntersection (a:as) bs

> setSubtraction []     []      = []
> setSubtraction []     (b:bs)  = []
> setSubtraction (a:as) []      = (a:as)
> setSubtraction (a:as) (b:bs) | a <  b  = a: setSubtraction as (b:bs)
>                              | a == b  = setSubtraction as bs
>                              | a > b   = setSubtraction (a:as) bs

> setElementOf x []            = False
> setElementOf x (y:ys)        = x==y || (x>y && setElementOf x ys)

> setUnionList = foldll setUnion setEmpty

% \section{Bags}
%
% The abstract data type of bags\index{bags} has the following signature.
%
% > abstype bag *
% > with bagUnion     :: bag * -> bag * -> bag *
% >      bagInsert    :: *     -> bag * -> bag *
% >      bagToList    :: bag * -> [*]
% >      bagFromList  :: [*]   -> bag *
% >      bagSingleton :: *     -> bag *
% >      bagEmpty     :: bag *
%
% \subsection{Representation}
%
% In this implementation, bags are represented by {\em unordered\/} lists.
%
% > bag * == [*]
%
% The implementation of the operations is straightforward.
%
% > bagUnion as bs = as ++ bs
% > bagInsert a as = a:as
% > bagToList xs   = xs
% > bagFromList xs = xs
% > bagSingleton x = [x]
% > bagEmpty       = []
%
\section{Other useful function definitions}
\label{sect:util-funs}

The definitions of @fst@ and @snd@ are present in later versions of
Miranda, but not earlier ones.  We always use @first@ and @second@
instead to avoid compatibility problems.

> first (a,b) = a
> second (a,b) = b

The function @zipWith@ zips together two lists, combining corresponding
elements with a given function.  The resulting list is as long as the shorter
of the two input lists.

> -- zipWith is defined in standard prelude

\par
The definition of @foldl@ differs between different versions of Miranda, so
we avoid the problem by writing our own function @foldll@, which does the
following:
GHiven a dyadic function
$\otimes$, a value $b$ and a list $xs\ =\ [x_1,...,x_n]$,
$@foldll@~ \otimes~ b~ xs$
computes
$( \ldots ((b~ \otimes~ x_1)~ \otimes~ x_2)~ \otimes~ \ldots x_n)$.
Section~\ref{sect:foldl-example} contains a
simple example of @foldll@ in action, together with a picture.

> foldll :: (a -> b -> a) -> a -> [b] -> a
> foldll = foldl                       -- in Gofer standard prelude.

\par
Finally, the function @mapAccuml@ is a rather useful combination of @map@
and @foldll@.
It is given a function, an accumulator\index{accumulator} and a list.
For each element of the list it applies the function to the current
accumulator and that list element, which gives a new value of the accumulator
and a new list element.  The result of @mapAccuml@ is the final value of
the accumulator, and the list of all the results.  The `@l@' in the
function name says that the accumulator is passed along from left to
right.
Section~\ref{sect:mapAccuml-example} has an example of @mapAccuml@ in
action, together with a picture.

> mapAccuml :: (a -> b -> (a, c)) -- Function of accumulator and element
>                                 --   input list, returning new
>                                 --   accumulator and element of result list
>              -> a               -- Initial accumulator
>              -> [b]             -- Input list
>              -> (a, [c])        -- Final accumulator and result list
>
> mapAccuml f acc []     = (acc, [])
> mapAccuml f acc (x:xs) = (acc2, x':xs')
>                          where (acc1, x')  = f acc x
>                                (acc2, xs') = mapAccuml f acc1 xs




> sort [] = []
> sort [x] = [x]
> sort (x:xs) = [ y | y <- xs, y < x] ++ x : [ y | y <- xs, y >= x ]

> space n = take n (repeat ' ')

