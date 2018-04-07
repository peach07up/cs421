MP5 - Scheme
============

Logistics
---------

-   revision: 1.0
-   due: July 20, 2016 (part 1)
-   due: July 27, 2016 (part 2)

This MP is broken into two parts, to be completed over two weeks. All of the
code we'll be supplying you with is present in your `app/Main.hs`, but we have
not fleshed out the entire `README.md` yet.

Objectives
----------

The objective for this MP is to build an interpreter for a small Lisp-like
language called Scheme. You will use the Parsec parser we went over in class to
build a parser, an evaluator to interpret the code, and a printer to show the
output. Together these will form your repl.

This language will have the normal things you would expect in a Lisp-like
language, such as functions, integers, and lists. You will also write a macro
system and explore how to use it. Macros give you the ability to program your
programming language, redefining it to be anything you want.

Goals
-----

-   Create the ADTs necessary to store a Scheme statement
-   Learn how to use parser combinators (specifically the Parsec library)
-   Understand the basic syntax of Scheme and how to evaluate programs in it
-   Create an REPL for your interpreter which handles manipulating the
    environment based on inputs from the user

Further Reading
---------------

We'll be using the `Parsec` parser combinator Haskell library extensively for
this MP. It would be good to read some of the documentation, and perhaps look at
some examples of how to use it. We've been supplying you with the parsers for
each MP up to this point, so you can also look at those and see how we have used
the `Parsec` library.

The [Haskell.org Parsec Page](https://wiki.haskell.org/Parsec) has good links to
a few tutorials and examples. As is often the case with excellently documented
Haskell libraries, the best resource is [the source
itself](https://hackage.haskell.org/package/parsec/docs/Text-Parsec.html). In
particular, the "Combinators" section of that page is quite useful.

Getting Started
===============

Relevant Files
--------------

In the file `app/Main.hs`, you'll find all the code that we supply you. The file
`test/Tests.hs` contains the tests.

Running Code
------------

As usual, you have to `stack init` (you only need to do this once).

To run your code, start GHCi with `stack ghci` (make sure to load the `Main`
module if `stack ghci` doesn't automatically). From here, you can test
individual functions, or you can run the REPL by calling `main`. Note that the
initial `$` and `>` are prompts.

``` {.sh}
$ stack ghci
 ... More Output ...
Prelude> :l Main
Ok, modules loaded: Main.
*Main> main
```

To run the REPL directly, build the executable with `stack build` and run it
with `stack exec main`.

Testing Your Code
-----------------

**The test set is not complete as of revision 1.0. Some of the tests are
implemented but most are not. Do not count on it for accurately telling you
whether you can take the ML or not.**

You are able to run the test-suite with `stack test`:

``` {.sh}
$ stack test
```

It will tell you which test-suites you pass, fail, and have exceptions on. To
see an individual test-suite (so you can run the tests yourself by hand to see
where the failure happens), look in the file `test/Spec.hs`.

It will also tell you whether you have passed enough of the tests to receive
credit for the ML. If you do not pass 60% of the tests on the MP, you will not
receive credit for the associated ML.

You can run individual test-sets by running `stack ghci` and loading the `Spec`
module with `:l Spec`. Then you can run the tests (specified in `test/Tests.hs`)
just by using the name of the test:

Look in the file `test/Tests.hs` to see which tests were run.

Given Code
==========

There is not much given code this time around. We will be needing the `Parsec`
library for the parser you'll write, along with the `HashMap` library for
storing the runtime environment.

``` {.haskell}
module Main where
```

``` {.haskell}
import Prelude hiding (lookup)
import System.IO (hFlush, hPutStr, hPutStrLn, hGetLine, stdin, stdout)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap, fromList, lookup, insert, union, empty)
```

Problems (Part 1)
=================

The problems for this MP are broken into two parts (with due-dates spread out
over two weeks). The whole MP is available though, so you can work on part 2
before its due-date.

Datatypes
---------

You *must match* our data-constructor specification *exactly*. If you add extra
data-constructors, omit some of them, or provide data-constructors which don't
meet the specification, your code will likely not compile with our tests.

### Environments

We will have a `runtime` environment which stores a map from variable names to
values (type `Val`). We've provided a type-synonym here for it:

``` {.haskell}
type Env = HashMap String Val
```

### Expressions

First, you'll need to implement the data-types which store the Scheme
expressions that are input.

``` {.haskell}
data Exp = IntExp Integer
         deriving (Show, Eq)
```

We've given you the first data-constructor, `IntExp :: Integer -> Exp`. You'll
need to provide the other two, `SymExp` and `SExp`.

-   `SymExp :: String -> Exp`
-   `SExp   :: [Exp]  -> Exp`

The data-constructor `SymExp` corresponds to a "symbol", which could be a
variable name, some defined function's name, or a primitive operator name.

The data-constructor `SExp` corresponds to a Scheme *s-expression*, which is at
the heart of how Scheme works. As you will see, *s-expressions* are used for
nearly everything in Scheme.

### Values

You'll also need to provide the data-constructors for the values we'll be using
in the Scheme language. We've provided the first two for you, `IntVal` and
`SymVal`. You must provide the rest.

``` {.haskell}
data Val = IntVal Integer
         | SymVal String
```

The remaning data-constructors should have types:

-   `ExnVal  :: String                 -> Val`
-   `PrimVal :: ([Val] -> Val)         -> Val`
-   `Closure :: [String] -> Exp -> Env -> Val`
-   `DefVal  :: String                 -> Val`
-   `ConsVal :: Val -> Val             -> Val`
-   `Macro   :: [String] -> Exp        -> Val`

`ExnVal` will be used to hold an error message so that the user can be signaled
that an error occurred. `PrimVal` is used to hold the definition of a primitive
operator in our language (such as `+`).[^1] `Closure` is used to store a
function definition. `DefVal` is used signal the REPL that a new binding should
be entered into the environment. `ConsVal` can be used for building both pairs
and lists in our Scheme. `Macro` is used to store the definition of a Scheme
macro (much like `Closure` stores the definition of a Scheme function.

You also need to provide a `Show` instance for the `Val` datatype so that we can
pretty-print things of type `Val`.

`IntVal`:
:   Give the string corresponding to the integer.

`SymVal`:
:   Give the string as-is.

`ExnVal`:
:   Given the string `"*** Scheme-Exception: "`, followed by the exception
    message, followed by `" ***"`.

`PrimVal`:
:   Give the string `"*primitive*"`.

`Closure`:
:   Give the string `"*closure*"`.

`DefVal`:
:   Give the name of the variable being defined.

`ConsVal`:

:   Give (in parenthesis) the list or tuple. If the list ends in `SymVal "nil"`,
    then it is a proper list, and should be output with spaces between the
    elements and an extra space at the end. If it does not end with
    `SymVal "nil"`, then it is a tuple and should be output with spaces between
    all elements except the last two, which should be output with a dot
    between them. For example:

    ``` {.haskell}
    ConsVal (IntVal 3) (ConsVal (SymVal "oeunt") (SymVal "nil"))
    ```

    is a list because it ends with `SymVal "nil"`. Thus it should be output as

    ``` {.scheme}
    (3 oeunt )
    ```

    But, the following is a tuple (because it doesn't end with `SymVal "nil"`):

    ``` {.haskell}
    ConsVal (IntVal 3) (ConsVal (SymVal "eohino") (SymVal "noethu"))
    ```

    so it should be output as:

    ``` {.scheme}
    (3 eohino . noethu)
    ```

`Macro`:
:   Give the string `"*macro*"`.

Parsing
-------

You must implement the parser for this MP. Scheme is a simple language though,
so it is not too difficult.[^2]

We start out with a prettier type for the parsers:

``` {.haskell}
type Parser = ParsecT String () Identity
```

This type synonym allows us to write the type `Parser Exp` if our parser will
read a `String` and return an `Exp`. Similarly, we could write `Parser Int` if
our parser will read a `String` and return an `Int`. The alternative would be to
write the entire type synonym each time, eg. `ParsecT String () Identity Exp` or
`ParsecT String () Identity Int`. That would be a pain and is uglier.

We are using the `Parsec` library to build parser combinators. You can use the
function `parseWith :: Parser a -> String -> Either ParseError a` given below to
test your parsers by hand.

``` {.haskell}
parseWith :: Parser a -> String -> Either ParseError a
parseWith parser input = parse parser "" input
```

For example, you can test if the `identifier :: Parser String` parser you have
to write works:

``` {.haskell}
*Main> parseWith identifier "eotnah"
Right "eotnah"
*Main> parseWith identifier "2222natuheant"
Left (line 1, column 1):
unexpected "2"
```

### Lexicals

Lexical parsers are useful for chunking up the input stream into tokens which
may have meaning in our programming language. These parsers will be of type
`Parser Char` or `Parser String` because they haven't added any new type
information to the underlying input stream. Later, we'll use grammatical parsers
to take these series of tokens and try to assign more information to them (by
actually producing things of type `Exp` for example).

We've provided two lexical parsers for you, `adigit`, and `digits`. The give you
a simple feel for how to use parser combinators to build more complex parsers
from simple ones.

We encourage you to look at the Parsec documentation to see what functions like\
`oneOf :: Stream s m Char => [Char] -> ParsecT s u m Char` or\
`many1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]`.\
Remember that we have type-synonyms for the `ParsecT` type, so the above types
could be more simply read as `oneOf :: [Char] -> Parser Char` and\
`many1 :: Parser a -> Parser [a]`.

``` {.haskell}
adigit :: Parser Char
adigit = oneOf ['0'..'9']

digits :: Parser String
digits = many1 adigit
```

#### Whitespace parser

You should define the `whitespace :: Parser String` lexical parser. All it does
is consume any whitespace it sees (white space being defined as spaces,
newlines, and tabs: `" \n\t"`). After each of your other parsers, it is useful
to call the `whitespace` parser which will get rid of empty spaces at the
beginning of the input so that the rest of the input can be parsed.

**Do not use the `spaces` parser that Parsec provides. Would you learn anything
doing that?**

#### Identifier parser

Identifiers represent symbols and variable names. The first character of an
identifier must be one of the set `"-*+/:'?><=!"`, plus the upper and lower-case
letters. The remaining characters can be from this set combined with the digits.

Define a parser for the first character of the identifiers (defined in the
paragraph above), and let’s call it `identFirst`. Also define one for the
following characters of the identifiers, `identRest`. Use `identFirst` and
`identRest` to build up the `identifier` parser.

### Grammaticals

Grammatical parsers add information relevant to our programming language to the
input stream (such as type information). While a programs' tokens all may be
valid (so that it passes the lexer), the way they are arranged into a program
may not be valid (it may not pass the parser).

We've provided a very simple grammatical parser for you, `anInt`. It uses the
`digits :: Parser String` parser to get a string of digits, and uses `read` on
the result to turn it into an actual `Integer`. Then it is wrapped in an
`IntExp` so that it is of type `Exp`. In this way, we can take meaningless
strings (like `"324234"`) and give them meaning in our programming languages
(eg. `IntExp 324234`).

``` {.haskell}
anInt :: Parser Exp
anInt = do d <- digits
           return $ IntExp (read d)
```

#### Parsing symbols

Identifiers are stored in the `SymExp` data-constructor, and we will call them
*symbols*. In Scheme, a symbol serves two purposes. As in most languages, it can
represent a variable. The evaluator will have an `env` parameter that allows us
to look up variables' values. A symbol can also be a value in its own right,
which we're not handling until later.

Make a *grammatical* parser `aSym` which uses the lexical parser `identifier` to
parse a symbol and return it as something of type `Exp`.

#### Parsing forms

In Scheme, a *form* starts with a parenthesis and a name (an identifier), then
some arguments, and then a closing parenthesis. Forms are used for everything in
this language. If the initial symbol in a form is not a reserved word, it is
taken to be a function or primitive operation. For the rest of this MP, when we
say "form", we are specifically referring to an s-expression with a specific
symbol in the first position.

Update your parser to handle forms by adding the `aForm` parser. You will need
to read an opening parenthesis, then a list of expressions, then a closing
parenthesis. Internally, this structure is called an *s-expression*. We will use
an `SExp` data-constructor (of type `[Exp] -> Exp`) to store them. Remember to
consume all the extra whitespace which may occur anywhere inside a form!

Hint: Looking ahead a bit, you'll see the `anExp` parser which is for parsing
expressions. Make sure you use that parser if you ever need to parse an
expression (which you need to do for `aForm`).

If you’ve implemented this correctly, you should be able to run the following
code (notice we are testing the parser directly; we are *not* inside the repl).

``` {.haskell}
*Main> parseWith aForm "(f 10 30 x)"
Right (SExp [SymExp "f",IntExp 10,IntExp 30,SymExp "x"])
```

#### Quotes, Quasi-Quotes, and UnQuotes

The quote operator tells Scheme to convert the next expression to a value, as a
symbol or a list. You can quote anything in Scheme.

There are two ways to quote something in Scheme, and you should supply both. The
long way is the special form `quote`. The shortcut way is the `'` operator. It
is often used as a shortcut for `(list ...)` (which we will define soon) but it
quotes all the arguments before they are evaluated.

The parser does not have to handle the long way, because that will already be
handled correctly if you are parsing forms correctly. However, the parser will
need to handle the short-form explicitely.

Add a parser `aQuote` to handle the `'` operator; any expression `'e` should be
parsed as `SExp [SymExp "quote", e]`. Remember to use the `anExp` parser if you
want to parse any expressions - also realize that this parser won't work until
you finish the `anExp` parser.

A quasi-quote in Scheme is like a quote that can be escaped by an unquote. The
long form for a quasi-quote is `(quasiquote <exp>)`, and will be handled
correctly by your `aForm` parser already. But you need to handle the short-form,
`` ` <exp> ``, so that `` `e `` should be parsed as
`SExp [SymExp "quasiquote", e]`. Write the parser `aQQuote` which parses the
short-form of a quasi-quote. Make sure to handle whitespace anywhere it can
appear.

You'll also need to handle un-quotes (which will be semantically paired with
quasi-quotes). The long form for unquotes is `(unquote <exp>)`, and the short
form is `, <exp>` (with a comma). Once again, you don't need to handle the long
form explicitly, just the short form. Write the parser `anUnquote` which parses
the short-form of a quasi-quote. Make sure to handle whitespace anywhere it can
appear.

Hint: You may find it more compact to write a parser
`mkQuote :: Char -> String -> Parser Exp` which can be used to make the
`aQuote`, `aQQuote`, and `anUnquote` parsers. This isn't necessary.

Finally, make the `anyQuote` parser, which will parse any of the quoted forms.
This parser should parse `aQuote` *or* `aQQuote` *or* `anUnquote`.

#### Expression Parser

Finally, you need to make the expression parser. The expression parser should be
able to parse any type of expression, including `anInt`, `aSym`, `aForm`, or
`anyQuote`. Note that **the order you try parsers in matters**. For good
measure, make sure you handle any whitespace before the expression or after the
expression. This way, anytime you use the `anExp` parser elsewhere, you also
know it will handle any whitespace around the expression.

Lifters/Lowerers
----------------

We have provided some translators to go between Scheme values and
Haskell values. They are `liftbool`, `lowerbool`, `liftint`, and `lowerint`.
These can help when defining the various operator lifters.

``` {.haskell}
liftbool :: Bool -> Val
liftbool False = SymVal "nil"
liftbool True  = SymVal "t"

lowerbool :: Val -> Bool
lowerbool (SymVal "nil") = False
lowerbool _              = True

liftint :: Integer -> Val
liftint = IntVal

lowerint :: Val -> Integer
lowerint (IntVal i) = i
lowerint _          = error "Cannot lower, not an IntVal!"
```

### Boolean operations

`liftBoolOp` should take an operator that works on lists of `Bool` and make it
work on list of `Val`. Then we can use it to make primitive boolean operators in
Scheme out of Haskell boolean operators.

### Integer operations

`liftIntOp` takes an operator and a base-case. If the supplied list of values is
empty, then the base-case (lifted into the Scheme world) is used. If it's
non-empty, then the operator is applied between all the elements of the list.

For example (note that this example is written in Scheme):

``` {.scheme}
(+ 3 4 8)       => 15
(- 5 2 7 12)    => -16
```

### Comparison operations

`liftCompOp` takes an integer comparison function in Haskell and lifts it to a
variadic Scheme comparison operator. If the list is empty or has a single
element, it should return Scheme's `True`. If the list is larger, it should
compare the elements of the list pair-wise using the given operator and then
logically `and` all of those together.

For example (note that this example is written in Scheme):

``` {.scheme}
(< 3 4 5)       => 't
(= 3 3 2)       => 'nil
(>= 3 4 2)      => 'nil
(>=)            => 't
(>= 7)          => 't
```

### List operations

Define functions `liftList :: [Val] -> Val` and `lowerList :: Val -> [Val]`
`liftList` should take a list of `Val` and turn it into a proper Scheme
cons-list. `lowerList` should take a proper Scheme cons-list and turn it into a
Haskell list of `Val`.

These functions should have the property that `lowerList . liftList = id`.

If a non-proper cons-list is lowered by `lowerList`, produce a Haskell `error`.

Problems (Part 2)
=================

Runtime
-------

The constant `runtime` is the initial runtime environment for the repl; it is a
map from `String` (identifiers) to `Val` (values). This will be used to hold the
values of defined constants, operators, and functions. You will call `repl` with
this `runtime` when running it.[^3]

You need to initialize `runtime` with predefined primitive operators as well.
This will make these operators available to users of your language. We have
supplied the tuple you will add to `runtime` for `+`.

``` {.haskell}
runtime :: Env
runtime = foldl union empty [ runtimeArith
                            , runtimeComp
                            , runtimeBool
                            , runtimeUnary
                            , runtimeOther
                            ]
```

### Arithmetic

Add integer subtraction (`-`) and multiplication (`*`) to your `runtime`
environment. You should use `liftIntOp` for these.

### Comparison

Add integer comparison operators to your `runtime` environment:

-   `>`: Integer greater than
-   `<`: Integer less than
-   `>=`: Integer greater than or equal
-   `<=`: Integer less than or equal
-   `=`: Integer equal
-   `!=`: Integer not equal

You should use `liftCompOp` for these.

### Boolean Operators

Add boolean operators `and` and `or` to your
`runtime environment. You should use`liftBoolOp\` for these.

### Unary Operators

We have three unary primitive operators in our Scheme:

-   `not`: Boolean not (only operates on first element)
-   `car`: Extract first element of a cons cell
-   `cdr`: Extract second element of a cons cell

Write a function `primUnary` which applies the given unary operator to a single
element of an input list, otherwise producing an `ExnVal` with the error message
"`opName` is a unary operator". Then use `primUnary` to define the above
operators. Use `primCar` and `primCdr` as the unary functions for `car` and
`cdr`. These should return an `Exnval` if the input is not a cons-cell.

### Other operators

There are two more primitive operators to define.

-   `eq?`: Integer and symbol equality
-   `list`: Construct a cons-list from arguments

Write the function `primEq` which will check if a list of `Val` are equal. For
an empty list or a singelton list, it should return Scheme's `True`. For
anything else it should check that all elements of the list are the same `Val`.

The `list` form can just use the `liftList` function you defined before. This
makes a Scheme list out of its arguments.

Evaluation
----------

Evaluation is where a Scheme expression is turned into a Scheme value.

### Check parameter names

### Quoting, Quasi-Quoting, and Unquoting

### Evaluation - the function!

#### Integer, Symbol, and Empty Forms

#### Variable Definition Forms

#### Function Definition and Lambda Function Forms

#### Quoting, Quasi-Quoting, and Unquoting Forms

#### Conditional Form

#### Let Form

#### Cons Form

#### Eval Form

#### Macro Form

#### Application Form

REPL
----

### Generating next environment

You'll need to define the `nextEnv` function, which returns a new environment
given a value. If the value is a `DefVal`, then `nextEnv` should insert the new
definition into the environment. If the value is anything else, the original
environment should be returned unchanged.

### Writing the REPL

Next you'll be defining the `repl :: Env -> IO ()`. You can use the two helper
functions below to assist in defining the `repl`.

``` {.haskell}
prompt :: String -> IO String
prompt str = hPutStr stdout str >> hFlush stdout >> hGetLine stdin

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout
```

The first function, `prompt`, will print a given string then wait for one line
of user input. You can access that line of input inside a `do` block using the
left-arrow `->`. The second function, `printLn`, will just print a string to the
console. Both functions have been made to immediately print their output,
instead of buffering it.

For example:

``` {.haskell}
    = do i1 <- prompt "input 1: "
         printLn $ "You typed '" ++ i1 ++ "'."
         i2 <- prompt "input 2: "
         printLn $ "Your input doubled is: '" ++ i2 ++ i2 ++ "'."
```

could lead to the following interaction:

    input 1: hello
    You typed 'hello'.
    input 2: world
    Your input doubled is: 'worldworld'.

Your `repl` needs to prompt the use for input, and check if the input is equal
to the string "quit". If so, then the `repl` should quit (by not recursing). If
not, then parse the input using `parseWith`. If it passes the parser, then the
resulting expression should be evaluated, printed, and `repl` should be called
recursively in the potentially updated environment. If it doesn't pass the
parser, the parse-error should be displayed and the `repl` called recursively in
the current environment.

### Main function

We've provided a `main` function for you, which just calls your `repl` with
`runtime` as the initial environment.

``` {.haskell}
main :: IO ()
main = do printLn "Welcome to your Scheme interpreter!"
          repl runtime
          printLn "Goodbye!"
```

[^1]: Scheme functions and primitives are *variadic*, which means they operate
    on lists of arguments. That is why `PrimVal` holds a function of type
    `[Val] -> Val` instead of `Val -> Val -> Val`.

[^2]: Scheme being a simple language does not mean it's not powerful. It has
    exactly the right simplicity to allow very elegant and compact
    specifications.

[^3]: You can call `repl` with any initial environment. `runtime` will just be a
    *default* initial environment to use.
