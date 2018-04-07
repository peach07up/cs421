module Main where

import Lib

import Text.Regex.TDFA

main :: IO ()
main = someFunc

isInt :: String -> Bool
isInt i = i =~ "[0-9]+"

isSymbol :: String -> String -> Bool
isSymbol s v = s == v

parseSymbol s (x:xs) =
  if s == x
     then (s,xs)
     else error $ "Parse error, expected " ++ s ++ " but got " ++ x ++ "."

-- Grammar
--
-- E -> + E E
--    | int
--    | var
--    | ( E )
--    | let var = E in E end

data Exp = PlusExp Exp Exp
         | IntExp Integer
         | VarExp String
         | LetExp String Exp Exp
    deriving Show

parse xx = parseE (words xx)

parseE ("+":xs) =
  let (e1,r1) = parseE xs
      (e2,r2) = parseE r1
   in (PlusExp e1 e2, r2)

parseE (x:xs) | isInt x =
                (IntExp (read x), xs)

parseE ("(":xs) =
  let (e1,r1) = parseE xs
      (_,r2) = parseSymbol ")" r1
   in (e1,r2)

       -- let var = E in E end

parseE ("let":xs) =
  let (v:r1) = xs
      (_,r2) = parseSymbol "=" r1
      (e1,r3) = parseE r2
      (_,r4) = parseSymbol "in" r3
      (e2,r5) = parseE r4
      (_,r6) = parseSymbol "end" r5
   in (LetExp v e1 e2, r5)

parseE (x:xs) = (VarExp x, xs)
