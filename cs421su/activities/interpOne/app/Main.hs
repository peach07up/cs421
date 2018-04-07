{-# LANGUAGE FlexibleContexts #-}

module Main where

import Text.Parsec

-- The Types

data Val = IntVal Integer
         | BoolVal Bool
         | ExnVal String
   deriving (Show,Eq)

data Exp = IntExp Integer
         | IntOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
   deriving (Show,Eq)

type Env = [(String,Val)]

-- The Parser
--
-- Treat this as a "black box"; you are not expected to understand it.  Yet.


-- Lexicals

int = do digits <- many1 digit <?> "An integer"
         spaces
         return (read digits :: Integer)

symbol s = do string s
              spaces
              return s

-- Expressions

varExp = do v <- many1 letter <?> "an indentifier"
            spaces
            return $ VarExp v

intExp = do i <- int
            return $ IntExp i

compOp =   do { symbol "<" ; return $ CompOpExp "<" }
       <|> do { symbol ">" ; return $ CompOpExp ">" }
       <|> do { symbol "<=" ; return $ CompOpExp "<=" }
       <|> do { symbol ">=" ; return $ CompOpExp ">=" }
       <|> do { symbol "/=" ; return $ CompOpExp "/=" }
       <|> do { symbol "==" ; return $ CompOpExp "==" }

mulOp =    do { symbol "*" ; return $ IntOpExp "*" }
       <|> do { symbol "/" ; return $ IntOpExp "/" }

addOp =    do { symbol "+" ; return $ IntOpExp "+" }
       <|> do { symbol "-" ; return $ IntOpExp "-" }

expr = arith `chainl1` compOp
arith = term `chainl1` addOp
term = factor `chainl1` mulOp
factor = atom
atom = intExp
     <|> varExp

-- Evaluator

intOps = [ ("+",(+))
         , ("-",(-))
         , ("*",(*))
         , ("/",div)
         , ("max",max)]

compOps = [ ("<",(<))
          , (">",(>))
          , ("==",(==))
          , ("<=",(<=))
          , (">=",(>=))
          , ("/=",(/=)) ]

liftIntOp f (IntVal i1) (IntVal i2) = IntVal (f i1 i2)
liftIntOp _ _ _ = ExnVal "Invalid type given to argument"

liftCompOp f (IntVal i1) (IntVal i2) = BoolVal (f i1 i2)
liftCompOp _ _ _ = ExnVal "Invalid type given to argument"

eval :: Exp -> Env -> Val
eval (IntExp i) _ = IntVal i
eval (VarExp s) env =
  case lookup s env of
    Just v -> v
    Nothing -> ExnVal "You Idiot!"

eval (IntOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op intOps
  in liftIntOp f v1 v2

eval (CompOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op compOps
  in liftCompOp f v1 v2

getInt (IntVal i) = i

-- REPL

repl :: Env -> IO ()
repl env =
  do putStr "interpreter> "
     input <- getLine
     case parse expr "stdin" input of
       Right exp -> let result = eval exp env in
                    do putStrLn (show result)
                       repl env
       Left msg -> do putStrLn $ show msg
                      repl env
main :: IO ()
main = repl []
