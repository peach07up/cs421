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
         | SymExp String
         | SExp [Exp]
         deriving (Show, Eq)

--- ### Values

data Val = IntVal Integer
         | SymVal String
         | ExnVal String 
         | PrimVal ([Val] -> Val)
         | Closure [String] Exp Env
         | DefVal String Val
         | ConsVal Val Val
         | Macro [String] Exp Env

		 
instance Show Val where
    -- show :: Val -> String
    show (IntVal i)         = show i
    show (SymVal s)         = s
    show (ExnVal e)         = e
    show (DefVal n a)       = n
    show (Closure a b c)    = "*closure*"
    show (PrimVal f)        = "*primitive*"
    show (ConsVal a b)      = "(" ++ (consHelper a b) ++ ")"
    show (Macro a b c )     = "*macro*"

consHelper a (ConsVal b c)  = show a ++ " " ++ consHelper b c
consHelper a (SymVal "nil") = show a ++ " "
consHelper a b              = show a ++ " . " ++ show b 

--instance Show Val where
--    show (IntVal i)         = show i
--    show (SymVal s)         = s
--    show (ExnVal e)         = "*** Scheme-Exception: " ++ s ++ " ***"
--    show (DefVal n _)       = n
--    show (Closure _ _ _)    = "*closure*"
--    show (PrimVal f)        = "*primitive*"
--    show l@(ConsVal _ _)      = "(" ++ (showCons l ++ ")"
--		where 
--			showCons (ConsVal car (SymVal "nil")) = show car ++ " "
--			showCons (ConsVal car l'@(ConsVal _ _)) = show car ++ " " ++ showCons l'
--			showCons (ConsVal car cdr)
--    show (Macro _ _ _)     = "*macro*"
	
---show = IntVal Integer| SymVal String|ExnVal String|PrimVal [Val] -> Val|Closure[String] -> Exp -> Env

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
whitespace = many (oneOf " \t\n")
---whitespace = many $ oneOf " \n\t"

--- #### Identifier parser

identFirst :: Parser Char
identFirst = oneOf $ "-*+/:'?><=!" ++ ['a'..'z'] ++ ['A'..'Z']

identRest :: Parser Char
identRest = identFirst <|> adigit

identifier :: Parser String
identifier = do s <- identFirst
                ss <- many identRest
                return $ s : ss 

--- ### Grammaticals

anInt :: Parser Exp
anInt = do  d <- digits
            return $ IntExp (read d)

--- #### Parsing symbols

---anExp :: Parser Exp
---anExp = do Text.Parsec.Prim.try $ do {whitespace}
---		anInt <|> aUnquote <|> aQuasiQuote <|> aQuote <|> aSym <|> aForm

aSym :: Parser Exp
aSym = do s <- identifier
          return $ SymExp s

--- #### Parsing forms
aForm :: Parser Exp
aForm = do oneOf "("
           whitespace
           pair <- many anExp ---(anExp >>= \p -> whitespace >> return p)
		   whitespace
           oneOf ")"
           return $ SExp pair

--- #### Quotes, Quasi-Quotes, and UnQuotes

aQuote :: Parser Exp
aQuote = mkQuote '\'' "quote"

aQuasiQuote :: Parser Exp
aQuasiQuote = mkQuote '`' "quasiquote"

aUnquote :: Parser Exp
aUnquote = mkQuote ',' "unquote"

--- optionally, the above can be defined in terms of `mkQuote`
mkQuote :: Char -> String -> Parser Exp
mkQuote c name = do Char c
					whitespace
					exp <- anExp
					return $ SExp [SymExp name, exp]

anyQuote :: Parser Exp
anyQuote = aQuote <|> aQuasiQuote <|> aUnquote

--- #### Expression Parser

anExp :: Parser Exp
anExp = do 	whitespace
			exp <- anyQuote <|> anInt <|> aSym <|> aForm
			whitespace
			return exp
			
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

liftBoolOp :: ([Bool] -> Bool) -> ([Val]) -> Val
liftBoolOp f = liftbool . f . map lowerbool

--- ### Integer operations

liftIntOp :: (Integer -> Integer -> Integer) -> Integer -> ([Val] -> Val)
liftIntOp _ o [] = liftint o
liftIntOp f z vs = liftint . foldl1 f . map lowerint vs

--- ### Comparison operations

liftCompOp :: (Integer -> Integer -> Bool) -> ([Val] -> Val)
liftCompOp _ [] = liftbool True
liftCompOp f vs = liftbool $ let vc = map lowerint vs
							 in and $ zipWith f vc (tail vc)

---liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
---liftCompOp _ _ _ = ExnVal "Cannot lift"

--- ### List operations
liftList :: [Val] -> Val
liftList = foldr ConsVal (liftbool False)

lowerList :: Val -> [Val]
lowerList (ConsVal car cdr) = car : lowerList cdr
lowerList (SymVal "nil") = []
lowerList _ = error "Cannot lower, not a list 'ConsVal'!"

---liftlist [] = SymVal "nil"
---liftlist (x:xs) = ConsVal x (liftlist xs)

---lowerlist (SymVal "nil") = []
---lowerlist (ConsVal a b) = a:(lowerlist b)
---lowerlist _ = error "Cannot lower, not a ConsVal!"


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
						  ("-", PrimVal $ liftIntOp (-) 0)
						  ("*", PrimVal $ liftIntOp (*) 1)
                        ]

--- ### Comparison

runtimeComp :: Env
runtimeComp = fromList [ (">", Primval $ liftCompOp (>))
						 ("<", Primval $ liftCompOp (<))
						 (">=", Primval $ liftCompOp (>=))
						 ("<=", Primval $ liftCompOp (<=))
						 ("=", Primval $ liftCompOp (==))
						 ("!=", Primval $ liftCompOp (/=))
                       ]

--- ### Boolean Operators

runtimeBool :: Env
runtimeBool = fromList [ ("and", Primval $ liftBoolOp and)
						,("or" Primval $ liftBoolOp or)
                       ]

--- ### Unary Operators

primNot :: Val -> Val
primNot = liftbool . not . lowerbool


primCar :: Val -> Val
primCar (ConsVal car cdr) = car
primCar val = ExnVal $ "Not a cons cell: " ++ show val

primCdr :: Val -> Val
primCar (ConsVal car cdr) = cdr
primCar val = ExnVal $ "Not a cons cell: " ++ show val
							  
primUnary :: String -> (Val -> Val) -> [Val] -> Val
primUnary _ f [v] = f v
primUnary opName _ _ = ExnVal $ "`" ++ opName ++ "` is a unary operator."

runtimeUnary :: Env
runtimeUnary = fromList [("not", PrimVal $ liftUnary "not" primNot)
						 ("car", PrimVal $ liftUnary "car" primCar)
						 ("cdr", PrimVal $ liftUnary "cdr" primCdr)
                        ]

--- ### Other operators

primEq :: [Val] -> Val
primEq [] = liftbool True
primEq vs = liftbool . and $ zipWith eqVal vs (tail vs)
			where
				eqVal (IntVal i1) (IntVal i2) = i1 == i2
				eqVal (SymVal s1) (SymVal s2) = s1 == s2
				eqVal (ConsVal car1 cdr1) (ConsVal car2 cdr2) = eqVal car1 car2 && eqVal cdr1 cdr2
				eqVal _ _ = False
				
---primEq ((IntVal _):(SymVal _):list) = liftbool False
---primEq ((SymVal _):(IntVal _):list) = liftbool False
---primEq ((IntVal x):(IntVal y):[]) = liftbool $ x == y
---primEq ((SymVal x):(SymVal y):[]) = liftbool $ x == y
---primEq ((IntVal x):(IntVal y):list) = liftbool $ (x == y) && (lowerbool (primEq $ (IntVal x):list))
---primEq ((SymVal x):(SymVal y):list) = liftbool $ (x == y) && (lowerbool (primEq $ (SymVal x):list))
---primEq x = SymVal "t"

runtimeOther :: Env
runtimeOther = fromList [ ("eq?", PrimVal $ primEq)
						 ,("list", PrimVal $ liftList)
                        ]

--- Evaluation
--- ----------

--- ### Check parameter names

paramStrs :: [Exp] -> Either String [String]
paramStrs = traverse paramStr
	where
		paramStr (SymExp p) = Right p
		paramStr _ = Left "Must use onlu `SymExp` for parameter names."

--- ### Quoting, Quasi-Quoting, and Unquoting

quote :: Exp -> Val
quote (IntExp i) = IntVal i
quote (SymExp s) = SymVal s
quote (SExp exps) = liftList $ map quote exps

---quote (SExp []) = SymVal "nil"
---quote (SExp (x:xs)) = ConsVal (quote x)  (quote (SExp xs))

quasiquote :: Exp -> Env -> Integer -> Val
quasiquote (SExp [SymExp "unquote", exp]) env 1
	= eval exp env
quasiquote (SExp l@[SymExp "unquote", _]) env d
	= liftList $ map (\e -> quasiquote e env (d-1)) l
quasiquote (SExp l@[SymExp "quasiquote", _]) env d
	= liftList $ map (\e -> quasiquote e env (d+1)) l
quasiquote (SExp l) env d
	= liftList $ map (\e -> quasiquote e env d) l
quasiquote x _ _ 
	= quote x
			

unquote :: Val -> Exp
unquote (IntVal i) = IntExp i
---unquote (SymVal "nil") = SExp []
unquote (SymVal s) = SymExp s
unquote l@(ConsVal _ _) = SExp $ (map unquote $ lowerlist l)

--- ### Evaluation - the function!

eval :: Exp -> Env -> Val


--- #### Integer, Symbol, and Empty Forms
eval (IntExp i) env                                                 
    = IntVal i
	
eval (SymExp s) env = 
    case H.lookup s env of
     Just v  -> v
     Nothing -> case s of
                    "unquote" -> ExnVal ("*** Scheme-Exception: Cannot `unquote` more than `quasiquote`.")
                    otherwise -> ExnVal ("*** Scheme-Exception: Symbol " ++ s ++ " has no value.")
					
eval (SExp []) env       = SymVal "nil"
					
--- #### Variable Definition Forms
eval (SExp ((SymExp "define"):rest)) env = case rest of
    (SymExp name):(SExp s):body:[]     -> case (all (\a -> case a of 
                                                             SymExp s  -> True
                                                             otherwise -> False) s) of
                                          True -> DefVal name closure
                                                  where closure = (Closure (map (\(SymExp a) -> a) s) body nenv)
                                                        nenv    = H.insert name closure env
														otherwise -> ExnVal "*** Scheme-Exception: Must use only `SymExp` for parameter names. ***"
														
--- #### Function Definition and Lambda Function Forms
eval (SExp ((SymExp "def"):rest)) env = case rest of
    (SymExp name):body:[]     -> DefVal name (eval body env)
    otherwise                 -> ExnVal "*** Scheme-Exception: Symbol def has no value. ***"
	

eval (SExp ((SymExp "lambda"):(SExp args):body:[])) env = (Closure (map (\(SymExp a) -> a) args) body env)
	
--- #### Quoting, Quasi-Quoting, and Unquoting Forms
eval (SExp ((SymExp "quote"):(e):[])) env = quote e

eval (SExp ((SymExp "quasiquote"):(e):[])) env = quasiquote e env 1

--- #### Conditional Form
eval (SExp ((SymExp "cond"):(SExp body):[])) env = evalCond body
     where evalCond (x:y:rest) = case eval x env of
            SymVal "nil" -> evalCond rest
            _            -> eval y env
			evalCond [] = SymVal "nil"
			
--- #### Let Form
eval (SExp ((SymExp "let"):(SExp body):e:[])) env = eval e (H.union (H.fromList letenv) env)
     where letenv = map (\[(SymExp s),e] -> (s, eval e env)) def
	 def = map (\(SExp x) -> x) body
	 
--- #### Cons Form
eval (SExp ((SymExp "cons"):x:y:[])) env = ConsVal (eval x env) (eval y env) 

--- #### Eval Form
eval (SExp ((SymExp "eval"):q:[])) env = eval (unquote $ eval q env) env

--- #### Macro Form
eval (SExp ((SymExp "defmacro"):(SymExp n):(SExp args):rest:[])) env = DefVal n closure
	where closure = (Macro (map (\(SymExp x) -> x) args) rest env)

--- #### Application Form
eval (SExp (s:rest)) env = 
    case (eval s env) of
        (PrimVal v) -> v $ map (\arg -> eval arg env) rest
        (Closure s e env') -> let evaluated = map (\a -> eval a env) rest
                              in eval e (H.union (H.fromList (zip s evaluated)) env')
        (Macro a e menv) -> eval (unquote $ eval e nenv) env
                        where nenv = H.union menv (H.fromList (zip a evaluated))
                              evaluated = lowerlist $ quasiquote (SExp rest) env 1
        (ExnVal s)  -> ExnVal s
        otherwise   -> otherwise
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
repl env =
    do  putStr "scheme> "
        l <- getLine                                                       
        case parse anExp "Expression" l of                                  
            Right exp -> case eval exp env of
                DefVal name cl@(Closure args exp env) -> do (putStrLn (name))
                                                            repl $ H.insert name cl env
                                                        where val = Closure args exp $ H.insert name cl env
                DefVal s val  -> do (putStrLn (s))
                                    repl $ H.insert s val env
                otherwise     -> putStrLn $ show (eval exp env)             
            Left pe   -> putStrLn (show pe)                                 
        repl env                                                            
		

--- ### Main function

main :: IO ()
main = do printLn "Welcome to your Scheme interpreter!"
          repl runtime
          printLn "Goodbye!"
		  