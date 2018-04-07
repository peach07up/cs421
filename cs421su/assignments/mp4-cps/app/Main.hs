--- Given Code
--- ==========

module Main where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         | IntExp Integer
         | VarExp String
         | LamExp String Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (AppExp f e)     = show f ++ " " ++ show e
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3 ++ ")"
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"

--- The Parser
--- ----------

-- Pretty parser type
type Parser = ParsecT String () Identity

--- ### Lexicals

symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

int :: Parser Integer
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Integer)

var :: Parser String
var = let keywords = ["if", "then", "else"]
      in  try $ do v1 <- letter                  <?> "an identifier"
                   vs <- many (letter <|> digit) <?> "an identifier"
                   spaces
                   let v = v1:vs
                   if (any (== v) keywords)
                    then fail "keyword"
                    else return v
                    
oper :: Parser String
oper = do op <- many1 (oneOf "+-*/<>=") <?> "an operator"
          spaces
          return op

parens :: Parser a -> Parser a
parens p = do symbol "("
              pp <- p
              symbol ")"
              return pp

--- ### Expressions

intExp :: Parser Exp
intExp = do i <- int
            return $ IntExp i

varExp :: Parser Exp
varExp = do v <- var
            return $ VarExp v

opExp :: String -> Parser (Exp -> Exp -> Exp)
opExp str = do symbol str
               return (OpExp str)

mulOp :: Parser (Exp -> Exp -> Exp)
mulOp = opExp "*" <|> opExp "/"

addOp :: Parser (Exp -> Exp -> Exp)
addOp = opExp "+" <|> opExp "-"

compOp :: Parser (Exp -> Exp -> Exp)
compOp =     opExp "<"  <|> opExp ">"
         <|> opExp "<=" <|> opExp ">="
         <|> opExp "/=" <|> opExp "=="

ifExp :: Parser Exp
ifExp = do try $ symbol "if"
           e1 <- expr
           symbol "then"
           e2 <- expr
           symbol "else"
           e3 <- expr
           return $ IfExp e1 e2 e3

lamExp :: Parser Exp
lamExp = do try $ symbol "\\"
            param <- var
            symbol "->"
            body <- expr
            return $ LamExp param body

appExp :: Parser Exp
appExp = do e1 <- expr
            e2 <- expr
            return $ AppExp e1 e2

atom :: Parser Exp
atom =     intExp
       <|> ifExp
       <|> lamExp
       <|> varExp
       <|> parens expr

expr :: Parser Exp
expr = let arith  = term `chainl1` addOp
           term   = factor `chainl1` mulOp
           factor = app
           app    = do f <- many1 atom
                       return $ foldl1 AppExp f
       in  arith `chainl1` compOp

parseExp :: String -> Either ParseError Exp
parseExp str = parse expr "stdin" str

--- ### Declarations

decl :: Parser Stmt
decl = do f <- var
          params <- many1 var
          symbol "="
          body <- expr
          return $ Decl f params body

parseDecl :: String -> Either ParseError Stmt
parseDecl str = parse decl "stdin" str

--- The REPL
--- --------

prompt :: String -> IO ()
prompt str = hPutStr stdout str >> hFlush stdout

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: IO ()
repl = do input <- prompt "> " >> getLine
          case input of
            "quit" -> return ()
            _      -> do case parseDecl input of
                            Left err    -> do printLn "Parse error!"
                                              printLn $ show err
                            Right decl  -> printLn . show $ cpsDecl decl
                         repl


main :: IO ()
main = do putStrLn "Welcome to the CPS Transformer!"
          repl
          putStrLn "GoodBye!"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 f = f 1
factk n f = factk (n-1) (\x -> f (x * n))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk (x:[]) e o
    | x `mod` 2 == 0 = e x
    | otherwise      = o x
evenoddk (x:xs) e o
    | x `mod` 2 == 0 = evenoddk xs (\y -> e (x + y)) o
    | otherwise      = evenoddk xs e (\y -> o (x + y))
--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### `isSimple :: Exp -> Bool`

isSimple :: Exp -> Bool
isSimple (IntExp _) = True
isSimple (VarExp _) = True
isSimple (AppExp _ _) = False
isSimple (OpExp _ exp1 exp2) = isSimple exp1 && isSimple exp2
isSimple (IfExp exp1 exp2 exp3) = isSimple exp1 && isSimple exp2 && isSimple exp3

--- ### `cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)`

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k n = (AppExp k (IntExp i), n)
cpsExp (VarExp v) k n = (AppExp k (VarExp v), n)

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f e) k n
    | isSimple e = ((AppExp (AppExp f e) k), n)
    | otherwise  = cpsExp e (LamExp v (AppExp (AppExp f (VarExp v)) k)) n2 
        where (v,n2) = gensym n


--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp cond exp1 exp2) k n
    | isSimple cond = (IfExp cond exp3 exp4, n3)
    | otherwise     = cpsExp cond (LamExp v (IfExp (VarExp v) exp3 exp4)) n4
        where (exp3, n2) = cpsExp exp1 k n
              (exp4, n3) = cpsExp exp2 k n2
              (v,n4) = gensym n3 
--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op exp1 exp2) k n
    | isSimple exp1 && isSimple exp2 = (AppExp k (OpExp op exp1 exp2), n)
    | isSimple exp2                  = cpsExp exp1 (LamExp v (AppExp k (OpExp op (VarExp v) exp2))) n2
    | isSimple exp1                  = cpsExp exp2 (LamExp v (AppExp k (OpExp op exp1 (VarExp v)))) n2
    | otherwise                      = cpsExp exp1 (LamExp v exp3) n4
        where (exp3,n4) = cpsExp exp2 (LamExp v2 (AppExp k (OpExp op (VarExp v) (VarExp v2)))) n3
              (v2,n3) = gensym n2
              (v,n2) = gensym n
--- ### `cpsDecl :: Stmt -> Stmt`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f xs e) = Decl f (xs ++ ["k"]) exp
	where (exp,_) = cpsExp e (VarExp "k") 0