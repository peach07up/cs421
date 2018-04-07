module Main where

import Data.List (intercalate)
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, fromList)

-- for stack underflow errors
underflow :: a
underflow = error "Value stack underflow!"

--- The Types
--- ---------

type ForthState = (IStack, CStack, Dictionary, Output)
type IStack     = [Integer]
type CStack     = [[String]]
type Dictionary = H.HashMap String [Entry]
type Output     = [String]

data Entry = Prim (IStack -> IStack)
           | Def [String]
           | Num Integer
           | Unknown String

instance Show Entry where
    show (Prim f)    = "Prim"
    show (Def s)     = show s
    show (Num i)     = show i
    show (Unknown s) = "Unknown: " ++ s

--- Dictionary Access
--- -----------------

-- handle input lookups (and integers)
dlookup :: String -> Dictionary -> Entry
dlookup word dict
    = case H.lookup word dict of
        Just (x:_) -> x
        _          -> case reads word of
                        [(i,"")] -> Num i
                        _        -> Unknown word

-- handle inserting things into the dictionary
dinsert :: String -> Entry -> Dictionary -> Dictionary
dinsert key val dict
    = case H.lookup key dict of
        Just vals -> H.insert key (val:vals) dict
        Nothing   -> H.insert key [val]      dict

--- Initial State
--- -------------

-- initial integer stack
initialIStack :: IStack
initialIStack = []

-- initial call stack
initialCStack :: CStack
initialCStack = []

-- initial output
initialOutput :: [String]
initialOutput = []

-- initial ForthState
initialForthState :: ForthState
initialForthState = (initialIStack, initialCStack, initialDictionary, initialOutput)

--- The Read-Eval-Print Loop
--- ------------------------

repl :: ForthState -> IO ()
repl state
    = do putStr "> "
         input <- getLine
         if input == "quit"
            then do putStrLn "Bye!"
                    return ()
            else let (is, cs, d, output) = eval (words input) state
                 in  do mapM_ putStrLn output
                        repl (is, cs, d, [])

main = do putStrLn "Welcome to your Forth interpreter!"
          repl initialForthState

--- Problems
--- ========

--- Lifters
--- -------

-- lift Haskell integer operations to Forth operations
liftIntOp :: (Integer -> Integer -> Integer) -> IStack -> IStack
liftIntOp op (x:y:xs) = (y `op` x) : xs
liftIntOp _  _        = underflow

-- lift Haskell integer comparisons to Forth operations
liftCompOp :: (Integer -> Integer -> Bool) -> IStack -> IStack
liftCompOp op (x:y:xs)
            | y `op` x  = -1 : xs
            | otherwise = 0 : xs
liftCompOp _ _          = underflow

--- The Dictionary
--- --------------

initialDictionary :: Dictionary
initialDictionary
    = H.fromList [ ("+", [Prim (liftIntOp (+))]),
                   ("-", [Prim (liftIntOp (-))]),
                   ("*", [Prim (liftIntOp (*))]),
                   ("/", [Prim (liftIntOp div)]),
                   ("<", [Prim (liftCompOp (<))]),
                   (">", [Prim (liftCompOp (>))]),
                   ("<=", [Prim (liftCompOp (<=))]),
                   (">=", [Prim (liftCompOp (>=))]),
                   ("==", [Prim (liftCompOp (==))]),
                   (">=", [Prim (liftCompOp (/=))])
                 ]


--- The Parser
--- ----------

-- get the first well-nested string of tokens and the rest
splitWellNested :: Eq a => (a, a) -> [a] -> ([a], [a])
splitWellNested (start,end) words = splitWN 0 [] words
    where
        splitWN 0 acc (word:rest)
            | word == end    = (reverse acc, rest)
        splitWN n acc (word:rest)
            | word == start  = splitWN (n+1) (word:acc) rest
            | word == end    = splitWN (n-1) (word:acc) rest
            | otherwise      = splitWN n     (word:acc) rest
        splitWN _ acc []     = (reverse acc, [])

-- ifs have an optional `else` which also must be well-nested
splitIf :: [String] -> ([String], [String], [String])
splitIf input = (ifBlock,elseBlock,rest)
    where (ifThen,rest) = (splitWellNested ("if","then") input)
          (ifBlock, elseBlock) = splitIfElse 0 [] ifThen 
          splitIfElse 0 acc (word:rest)
            | word == "else"    = (reverse acc, rest)
          splitIfElse n acc (word:rest)
            | word == "if"  = splitIfElse (n+1) (word:acc) rest
            | word == "then"    = splitIfElse (n-1) (word:acc) rest
            | otherwise      = splitIfElse n     (word:acc) rest
          splitIfElse _ acc []     = (reverse acc, [])

--- The Evaluator
--- -------------

eval :: [String] -> ForthState -> ForthState

-- empty input and empty call stack -> return current state
eval [] (istack, [],       dict, out) = (istack, [], dict, reverse out)

-- empty input and non-empty call stack -> pop element off call stack
eval [] (istack, c:cstack, dict, out) = eval c (istack, cstack, dict, out)

-- define `.`
eval (".":words) (i:istack, cstack, dict, out)
    = eval words (istack, cstack, dict, show i : out)
eval (".":_) _ = underflow

-- define `.S` (hint, use `intercalate` and `reverse`)
eval (".S":words) (istack, cstack, dict, out)
    = eval words (istack, cstack, dict, intercalate " " (map show (reverse istack)) : out)

-- define `dup`
eval ("dup":words) (i:istack, cstack, dict, out)
    = eval words (i:i:istack, cstack, dict, out)
eval ("dup":_) _ = underflow

-- define `swap`
eval ("swap":words) (i:j:istack, cstack, dict, out)
    = eval words (j:i:istack, cstack, dict, out)
eval ("swap":_) _ = underflow

-- define `drop`
eval ("drop":words) (i:istack, cstack, dict, out)
    = eval words (istack, cstack, dict, out)
eval ("drop":_) _ = underflow

-- define `rot`
eval ("rot":words) (i:j:k:istack, cstack, dict, out)
    = eval words (k:i:j:istack, cstack, dict, out)
eval ("rot":_) _ = underflow

-- define `: <word> <definition> ;` (hint, use `splitWellNested`)
eval (":":name:words) (istack, cstack, dict, out)
    | definition == [] = eval rest (istack, cstack, dict, out) 
    | otherwise        = eval rest (istack, cstack, dinsert name (Def definition) dict, out)
        where (definition,rest) = splitWellNested (":",";") words
eval (":":_) _ = underflow    

-- define `<cond> if <true branch> [else <false branch>] then` (hint, use `splitIf`)
eval ("if":words) (i:istack, cstack, dict, out)
    | i == 0    = eval (elseBlock ++ rest) (istack, cstack, dict, out)
    | otherwise = eval (ifBlock ++ rest) (istack, cstack, dict, out)
        where (ifBlock,elseBlock,rest) = splitIf words
eval ("if":_) _ = underflow

-- define `begin <body> again` (hint, use `splitWellNested`)
eval all@("begin":words) (istack, cstack, dict, out)
    = eval loopBody (istack, all:cstack, dict, out)
        where (loopBody,_) = splitWellNested ("begin","again") words

-- define `exit`
eval ("exit":words) (istack, c:cstack, dict, out)
    | c == []           = eval ("exit":words) (istack, cstack, dict, out)
    | head c == "begin" = eval [] (istack, rest:cstack, dict, out)
    | otherwise         = eval ("exit":words) (istack, cstack, dict, out)
        where (_,rest) = splitWellNested ("begin","again") (tail c)
eval ("exit":_) _ = underflow

-- otherwise it should be handled by `dlookup` to see if it's a `Num`, `Prim`,
-- `Def`, or `Unknown`
eval (word:words) (istack, cstack, dict, out)
    = case dlookup word dict of
        Def def   -> eval def   (istack,   words:cstack, dict, out)
        Prim f    -> eval words (f istack, cstack,       dict, out)
        Num i     -> eval words (i:istack, cstack,       dict, out)
        Unknown s -> eval [] (istack, [], dict, ("Unknown symbol: " ++ s) : out)
		