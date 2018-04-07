--- Given Code
--- ==========

module Main where

import Prelude hiding (lookup)
import System.IO (hFlush, hPutStr, hPutStrLn, hGetLine, stdin, stdout)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap, fromList, lookup, insert, union, empty)


--- Problems (Part 1)
--- =================

--- Datatypes
--- ---------

--- ### Environments

type Env = HashMap String Val

--- ### Expressions

data Exp = IntExp Integer
         deriving (Show, Eq)

--- ### Values

data Val = IntVal Integer
         | SymVal String

instance Show Val where
    show = undefined

--- Parsing
--- -------

type Parser = ParsecT String () Identity

parseWith :: Parser a -> String -> Either ParseError a
parseWith parser input = parse parser "" input

--- ### Lexicals

adigit :: Parser Char
adigit = oneOf ['0'..'9']

digits :: Parser String
digits = many1 adigit

--- #### Whitespace parser

whitespace :: Parser String
whitespace = undefined

--- #### Identifier parser

identFirst :: Parser Char
identFirst = undefined

identRest :: Parser Char
identRest = undefined

identifier :: Parser String
identifier = undefined

--- ### Grammaticals

anInt :: Parser Exp
anInt = do d <- digits
           return $ IntExp (read d)

--- #### Parsing symbols

aSym :: Parser Exp
aSym = undefined

--- #### Parsing forms

aForm :: Parser Exp
aForm = undefined

--- #### Quotes, Quasi-Quotes, and UnQuotes

aQuote :: Parser Exp
aQuote = undefined

aQQuote :: Parser Exp
aQQuote = undefined

anUnquote :: Parser Exp
anUnquote = undefined

--- optionally, the above can be defined in terms of `mkQuote`
mkQuote :: Char -> String -> Parser Exp
mkQuote = undefined

anyQuote :: Parser Exp
anyQuote = undefined

--- #### Expression Parser

anExp :: Parser Exp
anExp = undefined

--- Lifters/Lowerers
--- ----------------

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

--- ### Boolean operations

liftBoolOp :: ([Bool] -> Bool) -> [Val] -> Val
liftBoolOp = undefined

--- ### Integer operations

liftIntOp :: (Integer -> Integer -> Integer) -> Integer -> ([Val] -> Val)
liftIntOp = undefined

--- ### Comparison operations

liftCompOp :: (Integer -> Integer -> Bool) -> ([Val] -> Val)
liftCompOp = undefined

--- ### List operations

liftList :: [Val] -> Val
liftList = undefined

lowerList :: Val -> [Val]
lowerList = undefined


--- Problems (Part 2)
--- =================

--- Runtime
--- -------

runtime :: Env
runtime = foldl union empty [ runtimeArith
                            , runtimeComp
                            , runtimeBool
                            , runtimeUnary
                            , runtimeOther
                            ]

--- ### Arithmetic

runtimeArith :: Env
runtimeArith = fromList [ ("+", PrimVal $ liftIntOp (+) 0)
                        ]

--- ### Comparison

runtimeComp :: Env
runtimeComp = fromList [
                       ]

--- ### Boolean Operators

runtimeBool :: Env
runtimeBool = fromList [
                       ]

--- ### Unary Operators

primNot :: Val -> Val
primNot = undefined

primCar :: Val -> Val
primCar = undefined

primCdr :: Val -> Val
primCdr = undefined

primUnary :: String -> (Val -> Val) -> [Val] -> Val
primUnary = undefined

runtimeUnary :: Env
runtimeUnary = fromList [
                        ]

--- ### Other operators

primEq :: [Val] -> Val
primEq = undefined

runtimeOther :: Env
runtimeOther = fromList [
                        ]

--- Evaluation
--- ----------

--- ### Check parameter names

paramStrs :: [Exp] -> Either String [String]
paramStrs = undefined

--- ### Quoting, Quasi-Quoting, and Unquoting

quote :: Exp -> Val
quote = undefined

quasiquote :: Exp -> Env -> Integer -> Val
quasiquote = undefined

unquote :: Val -> Exp
unquote = undefined

--- ### Evaluation - the function!

eval :: Exp -> Env -> Val
eval = undefined

--- #### Integer, Symbol, and Empty Forms

--- #### Variable Definition Forms

--- #### Function Definition and Lambda Function Forms

--- #### Quoting, Quasi-Quoting, and Unquoting Forms

--- #### Conditional Form

--- #### Let Form

--- #### Cons Form

--- #### Eval Form

--- #### Macro Form

--- #### Application Form

--- REPL
--- ----

--- ### Generating next environment

nextEnv :: Env -> Val -> Env
nextEnv = undefined

--- ### Writing the REPL

prompt :: String -> IO String
prompt str = hPutStr stdout str >> hFlush stdout >> hGetLine stdin

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: Env -> IO ()
repl = undefined

--- ### Main function

main :: IO ()
main = do printLn "Welcome to your Scheme interpreter!"
          repl runtime
          printLn "Goodbye!"
