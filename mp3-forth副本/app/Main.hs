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
liftCompOp = undefined

--- The Dictionary
--- --------------

initialDictionary :: Dictionary
initialDictionary
    = H.fromList [ ("+", [Prim (liftIntOp (+))])
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
splitIf = undefined

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
eval (".":words) _ = underflow

-- define `.S` (hint, use `intercalate` and `reverse`)

-- define `dup`

-- define `swap`

-- define `drop`

-- define `rot`

-- define `: <word> <definition> ;` (hint, use `splitWellNested`)

-- define `<cond> if <true branch> [else <false branch>] then` (hint, use `splitIf`)

-- define `begin <body> again` (hint, use `splitWellNested`)

-- define `exit`

-- otherwise it should be handled by `dlookup` to see if it's a `Num`, `Prim`,
-- `Def`, or `Unknown`
eval (word:words) (istack, cstack, dict, out)
    = case dlookup word dict of
        Def def   -> eval def   (istack,   words:cstack, dict, out)
        Prim f    -> eval words (f istack, cstack,       dict, out)
        Num i     -> eval words (i:istack, cstack,       dict, out)
        Unknown s -> eval [] (istack, [], dict, ("Unknown symbol: " ++ s) : out)
