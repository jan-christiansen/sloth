\documentclass[a4paper]{article}


%include polycode.fmt
%include forall.fmt
%include greek.fmt


\usepackage{alltt}
\usepackage{color}
\usepackage{url}


\newcommand{\magenta}[1]{{\color{magenta}#1}}
\newcommand{\red}[1]{{\color{red}#1}}
\newcommand{\bslash}{\symbol{92}}


%if False
\begin{code}
module Example where
\end{code}
%endif


\begin{document}

\section{Introduction}

This is a literate Haskell file that contains several examples of
testing |Prelude| functions for minimal strictness. %
This file is intended to give an introduction to Sloth and it is
used as unit test for the implementation. %
You can generate a pdf from this file using
lhs2tex\footnote{\url{http://hackage.haskell.org/package/lhs2tex}} by
invoking {\tt lhs2TeX -o Examples.tex Examples.lhs --poly \& pdflatex
  Examples.tex}. %


\section{Booleans}

First we check whether the Boolean conjunction |(&&)| is minimally
strict. %
To use Sloth we have to import the module |Test.Sloth|. %
\begin{code}
import Test.Sloth
\end{code}
The function |strictCheck| takes a function and an integer as
arguments. %
The integer specifies the maximal size of the test cases that are
considered. %
The size of a test case (a value of a certain type) is the number of
constructors it contains. %
\begin{code}
conjunctionExample = strictCheck (&&) 10
\end{code}
\begin{alltt}
     Main> conjunctionExample
     Finished 5 tests.
\end{alltt}
As |strictCheck| does not state any counter-examples |(&&)| is
probably minimally strict. %
In this case the absence of a counter-example even proves that |(&&)|
is minimally strict as the domain of |(&&)| has only finitely many
elements and all elements have a size smaller than |10|. %

In contrast to Sloth, a tool called StrictCheck \cite{Chi06,Chi11}
that has been the inspiration for Sloth identifies |(&&)| as
unnecessarily strict. %
Sloth uses the same approach as StrictCheck to check whether a
function is unnecessarily strict with respect to a specific
argument. %
But, in contrast to StrictCheck, Sloth restrains the number of test
cases to propose sequential functions only. %
In contrast, StrictCheck proposes the parallel |and| as it is less
strict than |(&&)|. %

Next we check the Boolean instance of the function |(<=)|. %
\begin{code}
leExample = strictCheck ((<=) :: Bool -> Bool -> Bool) 10
\end{code}
\begin{alltt}
     Main> leExample
     2: \bslash{}False _ -> \red{True}
     Finished 7 tests.
\end{alltt}
Sloth states that this function is unnecessarily strict. %
It presents a test case and the proposed result of a minimally strict
implementation of |(<=)|. %
The underscore represents $\bot$ (the smallest element of the
corresponding cpo) which denotes an error or a non-terminating
expression. %
The test case {\tt \bslash{}False \symbol{95} -> \red{True}} denotes
that a minimally strict implementation of |(<=)| yields |True| if it
is applied to |False| and |undefined|. %
If we replace the sub-term that is highlighted red by |undefined| we
get the current result of the function. %
That is, we have |(<=) False undefined == undefined|. %

Besides |strictCheck|, Sloth provides the function |check| that
additionally takes a configuration as first argument. %
For example, we have |strictCheck = check defaultConfig| where
|defaultConfig| is a configuration provided by Sloth. %
A configuration is a record that, among others, contains a Boolean
field called |detailed| that specifies whether Sloth is suposed to
present detailed informations about test cases. %
For example, the following application yields detailed information
about the counter-example for |(<=)|. %
\begin{code}
detailedLeExample =
  check  ( defaultConfig { detailed = True } )
         ((<=) :: Bool -> Bool -> Bool) 10
\end{code}
\begin{alltt}
     Main> detailedLeExample
     2: Argument(s): False _
     Current Result: _
     Proposed Result: \red{True}
     Finished 7 tests.
\end{alltt}

The function |(<=) :: Bool -> Bool -> Bool| is unnecessarily strict
because it is defined by means of |compare :: Bool -> Bool ->
Ordering|, which checks whether its arguments are equal. %
Therefore, |(<=)| evaluates both arguments even if its first argument
is |False|. %
There are two possible implementations that are less strict than the
current implementation of |(<=)|. %
One implementation satisfies |(<=) False undefined == True| and the
other satisfies |(<=) undefined True == True|. %
Note that we cannot satisfy both without employing non-sequential
features like concurrency. %
If there are several ways of defining a less strict implementation
Sloth selects the one that uses a pattern matching order that is most
similar to the pattern matching order of the original
implementation. %
For example, |(<=) :: Bool -> Bool -> Ordering| first performs pattern
matching on its first argument and, therefore, Sloth proposes a
function which satisfies |(<=) False undefined == True|. %

Next we check the function |and :: [Bool] -> Bool|. %
Instead of using the function |strictCheck| we use |verboseCheck|
which additionally presents successful test cases. %
\begin{code}
andExample = verboseCheck and 3
\end{code}
\begin{alltt}
     Main> andExample
     1: \bslash_ -> _
     2: \bslash[] -> True
     3: \bslash(_:_) -> _
     4: \bslash(False:_) -> False
     5: \bslash(True:_) -> ?
     6: \bslash(True:[]) -> True
     7: \bslash(True:_:_) -> ?
     Finished 7 tests.
\end{alltt}
Sloth presents seven test cases and the results of |and| for the
corresponding arguments. %
As the application |and (False:undefined)| yields a total result |and|
cannot be too strict for this test case. %
By monotonicity |and| furthermore cannot be too strict for more
defined arguments. %
Therefore, Sloth does not consider test cases that are more defined
than |False:undefined|. %

The question marks in test cases five and seven state that Sloth
cannot decide whether the function is minimally strict for these test
cases or not. %
To check whether a function yields the correct result for a test case
Sloth considers the behaviour of the function for more defined
arguments. %
In the standard configuration Sloth yields a question mark if it
considers less then two more defined arguments. %

The record field called |minInfSize| specifies the number of more
defined arguments Sloth has to consider. %
The following checks the function |and| and demands Sloth to use 20
more defined arguments. %
\begin{code}
andExample20 = check (verboseConfig { minInfSize = 20 }) and 3
\end{code}
\begin{alltt}
     Main> andExample20
     1: \bslash_ -> _
     2: \bslash[] -> True
     3: \bslash(_:_) -> _
     4: \bslash(False:_) -> False
     5: \bslash(True:_) -> ?
     6: \bslash(True:[]) -> True
     7: \bslash(True:_:_) -> ?
     Finished 7 tests.
\end{alltt}
Surprisingly Sloth yields the same results as before. %
The parameter |minInfSize| only affects test cases which Sloth is
uncertain about. %
That is, in the test cases one to four Sloth definitely knows that
the behaviour of |and| is minimally strict without considering 20 more
defined test cases. %

If we set the parameter |minInfSize| to one the output of Sloth
changes. %
\begin{code}
andExample1 = check (verboseConfig { minInfSize = 1 }) and 3
\end{code}
\begin{alltt}
     Main> andExample0
     1: \bslash_ -> _
     2: \bslash[] -> True
     3: \bslash(_:_) -> _
     4: \bslash(False:_) -> False
     5: \bslash(True:_) -> \magenta{True}
     6: \bslash(True:[]) -> True
     7: \bslash(True:_:_) -> ?
     Finished 7 tests.
\end{alltt}
First of all Sloth still yields a question mark for test case seven. %
In this case there are no values more defined than the test case and
Sloth always prints a question mark in this case. %
In contrast, test case five now is a potential counter-example. %
Sloth distinguishes \magenta{potential} counter-examples and
\red{definite} counter-examples. %
If a counter-example is potential it might be no counter-example if we
consider test cases of larger sizes. %
For example, if we increase the size of the previous test from three
to four Sloth does not identify |True:undefined| as counter-example. %
We will return to the difference between potential and definite
counter-examples later. %


\section{Polymorphism}

To check a polymorphic function we use the monomorphic instance where
all type variables are instantiated with the opaque data type |A| that
is provided by Sloth. %
Sloth uses symbolic values as test cases of type |A|. %
For example, if we check a function whose argument type is |[A]| Sloth
generates test cases |undefined|, |[]|, |a:undefined|, |a:[]|,
|a:b:undefined|, \dots{} where |a| and |b| are representing arbitrary
values of equal type. %
To test a function that takes an argument of type |[A]| Sloth in fact
replaces elements of type |A| by distinct integers, that is, it
generates test cases |undefined|, |[]|, |0:undefined|, |0:[]|,
|0:1:undefined|. %
These integers are displayed as variables as they actually represent
arbitrary values (for more details see \cite{ChrSei11}). %

The following application checks whether the polymorphic identity is
minimally strict.
\begin{code}
idExample = verboseCheck (id :: A -> A) 10
\end{code}
\begin{alltt}
     Main> idExample
     1: \bslash{}a -> a
     Finished 1 tests.
\end{alltt}
We employ properties of polymorphic functions that are guaranteed by
free theorems to increase the efficiency of testing polymorphic
functions. %
For example, |id| is only checked for the test case |a|, that is, for
|0|. %
Note that even a test of the simplest monomorphic instance |id :: ()
-> ()| generates two test cases, namely, |undefined| and |()|. %

In fact, in contrast to |id :: alpha -> alpha|, the function |id :: ()
-> ()| is unnecessarily strict. %
That is, we may not check the monophonic unit instance to check
whether the polymorphic function is minimally strict. %
\begin{code}
unitIdExample = verboseCheck (id :: () -> ()) 10
\end{code}
\begin{alltt}
     Main> unitIdExample
     1: \bslash_ -> \red{()}
     2: \bslash() -> ()
     Finished 2 tests.
\end{alltt}
The function |id :: () -> ()| yields |()| for all total arguments. %
That is, the function does not have to check its argument. %
In other words, the function |const ()| is less strict than |id :: ()
-> ()|. %

If we check the strict evaluation primitive |seq|\footnote{The
  function |seq :: alpha -> beta -> beta| satisfies the laws |seq
  undefined y == undefined| and |seq x y == y| if |x /= undefined|.}
Sloth states that it is minimally strict. %
\begin{code}
seqExample = verboseCheck (seq :: A -> A -> A) 10
\end{code}
\begin{alltt}
     Main> seqExample
     1: \bslash{}a b -> b
     Finished 1 tests.
\end{alltt}
This is quite surprising as |seq| is by definition unnecessarily
strict. %
For example, the function |\ _ y -> y| is less strict than |seq|. %
Sloth does not observe that |seq| is unnecessarily strict because it
only considers checks the application |seq a b|, that is, |seq 0 1|. %
If we instantiate the type variables of the type of |seq| by |Bool|
the result looks more promissing. %
\begin{code}
boolSeqExample = strictCheck (seq :: Bool -> Bool -> Bool) 10
\end{code}
\begin{alltt}
     Main> boolSeqExample 10
     2: \bslash_ False -> \red{False}
     3: \bslash_ True -> \red{True}
     Finished 7 tests.
\end{alltt}

The results of Sloth for |A| instances might be wrong if the
polymorphic function uses |seq|. %
The behaviour of Sloth for the |A| type is based on free theorems that
fail in the presence of |seq|. %
It is possible to handle polymorphic functions that use |seq|
correctly but we have to check significantly more test cases in this
case. %
Therefore, we expect that polymorphic functions do not use |seq| to
keep the number of test cases small (see \cite{ChrSei11} for more
details). %


\section{Lists}

In this chapter we consider functions on lists. %
\begin{code}
appendExample = strictCheck ((++) :: [A] -> [A] -> [A]) 10
\end{code}
\begin{alltt}
     Main> appendExample
     Finished 91 tests.
\end{alltt}
The function |(++)| is minimally strict. %
Furthermore note that the number of test cases is quite small if we
consider that we check |(++)| for all pairs of lists up to size ten. %
This is due to the fact that |(++)| is polymorphic and cannot be too
strict in the polymorphic component. %

As examples for minimally strict function we check |tail| and |zip|. %
\begin{code}
tailExample = strictCheck (tail :: [A] -> [A]) 100
\end{code}
\begin{alltt}
     Main> tailExample
     Finished 201 tests.
\end{alltt}
\begin{code}
zipExample = strictCheck (zip :: [A] -> [A] -> [(A,A)]) 100
\end{code}
\begin{alltt}
     Main> zipExample
     Finished 199 tests.
\end{alltt}

In contrast to |zip| the function |unzip| is not minimally strict. %
\begin{code}
unzipExample = strictCheck (unzip :: [(A,A)] -> ([A],[A])) 3
\end{code}
\begin{alltt}
     Main> unzipExample
     1: \bslash_ -> \red{(_,_)}
     3: \bslash(_:_) -> \red{(_:_,_:_)}
     Finished 8 tests.
\end{alltt}
The first counter-example states that |unzip| yields a tuple in all
cases and, therefore, should yield a tuple even if its argument is
|undefined|. %
To unterstand the other counter-example we consider the implementation
of |unzip|. %
\begin{spec}
unzip :: [(alpha,beta)] -> ([alpha],[beta])
unzip = foldr (\ (a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])
\end{spec}
The second counter-example states that |unzip (undefined:undefined) =
undefined| while we can have |unzip (undefined:undefined) =
(undefined:undefined,undefined:undefined)|. %
That is, if we apply |unzip| to a list with at least one element the
result is always a tuple where both lists have at least one element. %
In other words, the functional argument of |foldr| performs pattern
matching on its for argument. %
That is, |unzip| is head-strict although it does not have to be. %

If we replace the strict tuple matching of the functional argument of
|foldr| by a lazy matching we get a less strict implementation of
|unzip|. %
To satisfy the first counter-example we use a lazy pattern matching by
means of a |where| clause and explicitly construct the tuple. %
\begin{code}
unzip' :: [(alpha,beta)] -> ([alpha],[beta])
unzip' xs = (ys,zs)
  where
   (ys,zs) = foldr (\ ~(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[]) xs
\end{code}
If we check this less strict implementation, Sloth does not state any
counter-examples. %
\begin{code}
unzip'Example = strictCheck (unzip' :: [(A,A)] -> ([A],[A])) 100
\end{code}
\begin{alltt}
     Main> unzip'Example
     Finished 599 tests.
\end{alltt}

If we check whether |reverse| is minimally strict, Sloth states a
couple of potential counter-examples. %
\begin{code}
reverseExample = strictCheck (reverse :: [A] -> [A]) 5
\end{code}
\begin{alltt}
     Main> reverseExample
     3: \bslash(a:_) -> \magenta{_:_}
     5: \bslash(a:b:_) -> \magenta{_:_:_}
     7: \bslash(a:b:c:_) -> \magenta{_:_:_:_}
     Finished 11 tests.
\end{alltt}
We can confirm that a potential counter-example is indeed a
counter-example by checking |reverse| for inputs of larger sizes. %
If we check |reverse| for polymorphic lists up to size ten all
potential counter-examples for size five stay potential
counter-examples. %
As these potential counter-examples may still be no counter-examples,
we can verify a potential counter-example by hand. %
A potential counter-example is definitely a counter-example if all
more defined total inputs lead to results that are at least as defined
as the recommended result. %
For example, consider the first counter-example. %
For all total inputs that are more defined than |a:undefined|, that
is, for all lists with at least one element, the function |reverse|
yields a list with at least one element. %
A list with at least one element is at least as defined as the
recommended result |undefined:undefined|. %
Therefore, the first counter-example is definitely a
counter-example. %

A less strict implementation of |reverse| is only advantageous in very
uncommon use cases. %
For example, the evaluation of |null (reverse xs)| completely
evaluates the spine of |xs| while a minimally strict implementation of
|reverse| would only evaluate |xs| to head normal form in this case. %

As two more interesting examples we consider the functions
|intersperse| and |inits| from |Data.List|\footnote{We consider the
  definitions of |intersperse| and |inits| from \texttt{base-4.3.1.0}
  as these functions have been improved in \texttt{base-4.4.0.0}}. %
\begin{code}
intersperseExample = strictCheck (intersperse :: A -> [A] -> [A]) 4
\end{code}
\begin{alltt}
     Main> intersperseExample
     3: \bslash{}a (b:_) -> \red{a:_}
     5: \bslash{}a (b:c:_) -> b:a:\red{c:_}
     Finished 7 tests.
\end{alltt}
This nicely demonstrates the power of the symbolic testing of
polymorphic functions. %
For example, the first counter-example does not only state that
|intersperse| is too strict if it is applied to a non-terminated list
but also that the first element of the list is supposed to be the
element of the second argument. %
The unnecessarily strict implementation of |intersperse| can cause a
serious space leak as it is shown in \cite{Chr11}. %

The implementation of |intersperse| is unnecessarily strict because
the function checks whether the list has exactly one element. %
\begin{code}
intersperse :: alpha -> [alpha] -> [alpha]
intersperse  _    []      = []
intersperse  _    [x]     = [x]
intersperse  sep  (x:xs)  = x : sep : intersperse sep xs
\end{code}
As the first element of the result list is |x| no matter whether the
list has one element or more we can ``delay'' the pattern matching as
follows. %
\begin{code}
intersperse' :: alpha -> [alpha] -> [alpha]
intersperse' _    []      = []
intersperse' sep  (x:xs)  = x:go xs 
  where  
    go []      = []
    go (y:ys)  = sep : y : go ys
\end{code}

The implementation of |inits| is in a similar manner unnecessarily
strict. %
\begin{code}
initsExample = strictCheck (inits :: [A] -> [[A]]) 3
\end{code}
\begin{alltt}
     Main> initsExample
     1: \bslash_ -> \red{[]:_}
     3: \bslash(a:_) -> []:\red{(a:[]):_}
     5: \bslash(a:b:_) -> []:(a:[]):\red{(a:b:[]):_}
     Finished 7 tests.
\end{alltt}
\begin{code}
inits :: [alpha] -> [[alpha]]
inits []      =  [[]]
inits (x:xs)  =  [[]] ++ map (x:) (inits xs)
\end{code}
To define a less strict implementation we delay the pattern matching
that checks wether the argument is the empty list or not. %
\begin{code}
inits' :: [alpha] -> [[alpha]]
inits' xxs =
  [] : case  xxs of
             []    -> []
             x:xs  -> map (x:) (inits' xs) 
\end{code}
The function |tails :: [alpha] -> [[alpha]]| shows a similar behaviour
as |inits|, that is, it is unnecessarily strict. %


\section{Integers}

Using Sloth we can check functions that use integers. %
For example, the following application checks whether the
multiplication of two |Int|s is minimally strict. %
\begin{code}
multExample = strictCheck ((*) :: Int -> Int -> Int) 5
\end{code}
\begin{alltt}
     Main> multExample
     2: \bslash0 _ -> \magenta{0}
     Finished 21 tests.
\end{alltt}
As the counter-example shows the multiplication is too strict. %
If the first argument is zero we do not have to evaluate the second
argument. %
But obviously the primitive multiplication evaluates both arguments. %

When we check the monomorphic integer instance of |id| we can
observe the correspondence between the size parameter and the integers
considered as test cases. %
\begin{code}
intIdExample = verboseCheck (id :: Int -> Int) 4
\end{code}
\begin{alltt}
     Main> intIdExample
     1: \bslash_ -> _
     2: \bslash0 -> 0
     3: \bslash-1 -> -1
     4: \bslash1 -> 1
     5: \bslash-2 -> -2
     6: \bslash2 -> 2
     7: \bslash-3 -> -3
     8: \bslash3 -> 3
     Finished 8 tests.
\end{alltt}
The number of test cases is linear in the size, more precisely we
check the integers from $-\text{\it size-1}$ to $\text{\it size-1}$ if
we check |id :: Int -> Int| up to size \textit{size}. %

If we check |drop| for test cases up to size ten Sloth presents a
couple of potential counter-examples. %
\begin{code}
dropExample = strictCheck (drop :: Int -> [A] -> [A]) 10
\end{code}
\begin{alltt}
     Main> dropExample
     42: \bslash4 _ -> \magenta{[]}
     61: \bslash4 (a:_) -> \magenta{[]}
     62: \bslash5 _ -> \magenta{[]}
     83: \bslash4 (a:b:_) -> \magenta{[]}
     85: \bslash5 (a:_) -> \magenta{[]}
     86: \bslash6 _ -> \magenta{[]}
     Finished 146 tests.
\end{alltt}
If we check |drop| for test cases up to size eleven some of the
counter-examples disappear while additional potential counter-examples
appear. %
All these potential counter-examples are no counter-examples. %
For example, let us consider the first one. %
Sloth suspects that |drop| always yields the empty list if the first
argument is four. %
The lists generated by Sloth do not exceed length four. %
Therefore, the result of all considered test cases is indeed the empty
list. %
This example nicely demonstrates that not all potential
counter-examples are indeed counter-examples. %

As examples for minimally strict functions we check |(!!)| and
|take|. %
\begin{code}
indexingExample = strictCheck ((!!) :: [A] -> Int -> A) 100
\end{code}
\begin{alltt}
     Main> indexingExample
     Finished 5098 tests.
\end{alltt}
\begin{code}
takeExample = strictCheck (take :: Int -> [A] -> [A]) 100
\end{code}
\begin{alltt}
     Main> takeExample
     Finished 5000 tests.
\end{alltt}


\section{Character}

Sloth handles characters in a similar way as integers. %
That is, it does not enumerate all characters for test cases of size
one. %
The following example demonstrates the enumeration of characters by
employing the |Char| instance of |id|. %
\begin{code}
charIdExample = verboseCheck (id :: Char -> Char) 4
\end{code}
\begin{alltt}
     1: \bslash_ -> _
     2: \bslash'?' -> '?'
     3: \bslash'>' -> '>'
     4: \bslash'@@' -> '@@'
     5: \bslash'=' -> '='
     6: \bslash'<' -> '<'
     7: \bslash'A' -> 'A'
     8: \bslash'B' -> 'B'
     9: \bslash';' -> ';'
     10: \bslash':' -> ':'
     11: \bslash'9' -> '9'
     12: \bslash'8' -> '8'
     13: \bslash'C' -> 'C'
     14: \bslash'D' -> 'D'
     15: \bslash'E' -> 'E'
     16: \bslash'F' -> 'F'
\end{alltt}
In contrast to the enumeration of integers the emumeration of
characters is exponential in the size. %
That is, Sloth checks |id :: Char -> Char| for $2^{\text{\it size}}$
test cases if we check it for inputs up to size \textit{size}. %

To demonstrate one of the limitations of Sloth we check whether the
function |words| is unnecessarily strict. %
It is very difficult to check |words| because it yields a singleton
list as result unless the argument contains a newline character. %
As there are many characters it is not very likely that a test case
contains this character. %
Therefore, Sloth very likely assumes that |words| always yields lists
with only a single element as result. %
For example, if we check |words| for strings up to size ten Sloth
reports many potential counter-examples. %

Because there are more test cases that are more defined, potential
counter-examples that are reported first are more likely to be
counter-examples than counter-examples reported later. %
That is, we would like to check |words| for strings up to a large size
but only consider the first few counter-examples. %
For this purpose Sloth provides a function called |interactCheck|,
which interactively asks the user whether to present more
counter-examples. %
\begin{code}
wordsExample = interactCheck words 15
\end{code}
\begin{alltt}
     Main> wordsExample
     175: Argument(s): \bslash('?':'?':'>':_)
     Current Result: ('?':'?':'>':_):_
     Proposed Result: ('?':'?':'>':_):\magenta{[]}
     More? [y(es)/n(o)/a(ll)]n
\end{alltt}
If we check |words| for strings up to size 15 test case 175 is a
potential counter-example. %
By default |interactCheck| uses the detailed presentation of test
cases. %

At last we want to present one unnecessarily strict function that, at
first sight, might be surprising. %
If we check the Boolean instance of the |show| function, Sloth states
that it is unnecessarily strict. %
\begin{code}
showExample = strictCheck (show :: Bool -> String) 5
\end{code}
\begin{alltt}
     Main> showExample
     1: \bslash_ -> \red{_:_:_:_:_}
     Finished 3 tests.
\end{alltt}
As both possible results, the string |"True"| as well as the string
|"False"|, have at least four characters |show :: Bool -> String|
always yields a list with at least four elements. %
Therefore, Sloth proposes to yield a list structure with at least four
elements without evaluating the argument. %


\begin{thebibliography}{1}
\providecommand{\natexlab}[1]{#1}
\providecommand{\url}[1]{\texttt{#1}}
\expandafter\ifx\csname urlstyle\endcsname\relax
  \providecommand{\doi}[1]{doi: #1}\else
  \providecommand{\doi}{doi: \begingroup \urlstyle{rm}\Url}\fi

\bibitem[1]{Chi06}
O.\ Chitil.
\newblock Promoting non-strict programming.
\newblock In \emph{IFL'06 Draft Proceedings}, 2006.

\bibitem[2]{Chi11}
O.\ Chitil.
\newblock {StrictCheck}: a tool for testing whether a function is unnecessarily
  strict.
\newblock Technical Report 2-11, University of Kent, School of Computing, June
  2011.

\bibitem[3]{Chr11}
J.\ Christiansen.
\newblock {Sloth - A Tool for Checking Minimal Strictness}.
\newblock In \emph{PADL'11 Proceedings}. Springer, 2011.

\bibitem[4]{ChrSei11}
J.\ Christiansen and D.\ Seidel.
\newblock Minimally strict polymorphic functions.
\newblock In \emph{PPDP'11 Proceedings}. ACM, 2011.
\end{thebibliography}


\end{document}


%%% Local Variables: 
%%% mode: latex
%%% TeX-command-default: "lhs2tex"
%%% TeX-master: t
%%% End: 
